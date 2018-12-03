library(tidyverse)
library(data.table)
library(magrittr)
library(caret)
library(beepr)

load('datos/bases_airbnb.RData')

datos <- data.frame(y, x)

library(gbm)
mod_boosting <- gbm(y ~ .,
                    data = datos,
                    n.trees = 150, 
                    interaction.depth = 3,
                    shrinkage = 0.1, # tasa de aprendizaje
                    n.minobsinnode = 10,
                    bag.fraction = 1,
                    train.fraction = 0.75)

mod_boosting$valid.error %>% tail
mod_boosting$train.error %>% tail
summary(mod_boosting)


### Predicciones y error en log

datos.test <- data.frame(y.test, x.test)

df <- data.frame(pred = predict(mod_boosting, datos.test), 
                 obs = prueba$price,
                 country = prueba$country) %>% 
  mutate(residuo = obs - pred)

sum((df$obs - df$pred)^2) / nrow(df)

qqnorm(df$residuo)
qqline(df$residuo)

df %>% 
  ggplot(aes(obs, pred)) +
  geom_point(aes(color = country), size = 0.1) +
  geom_abline() +
  coord_equal() +
  scale_x_continuous(labels = scales::comma) +
  scale_y_continuous(labels = scales::comma) +
  theme_minimal()


df %>% 
  ggplot(aes(residuo)) +
  geom_density(color = 'red', size = 1, fill = 'red', alpha = 0.2) +
  geom_histogram(aes(y = ..density..), fill = 'royal blue', alpha = 0.75) +
  scale_x_continuous(labels = scales::comma) +
  # scale_y_continuous(labels = scales::comma) +
  theme_minimal()

### Predicciones y error en exp
df.exp <- data.frame(pred = exp(predict(mod_boosting, datos.test)), 
                     obs = exp(prueba$price),
                     country = prueba$country) %>% 
  mutate(residuo = obs - pred)

sum((df.exp$obs - df.exp$pred)^2) / nrow(df.exp)

qqnorm(df.exp$residuo)
qqline(df.exp$residuo)

df.exp %>% 
  ggplot(aes(obs, pred)) +
  geom_point(aes(color = country), size = 0.1) +
  geom_abline() +
  coord_equal() +
  scale_x_continuous(labels = scales::comma) +
  scale_y_continuous(labels = scales::comma) +
  theme_minimal()


df.exp %>% 
  ggplot(aes(residuo)) +
  geom_density(color = 'red', size = 1, fill = 'red', alpha = 0.2) +
  geom_histogram(aes(y = ..density..), fill = 'royal blue', alpha = 0.75) +
  scale_x_continuous(labels = scales::comma) +
  # scale_y_continuous(labels = scales::comma) +
  theme_minimal()
