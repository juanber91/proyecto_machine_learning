library(tidyverse)
library(data.table)
library(magrittr)


# Modelo lineal -----------------------------------------------------------
modelo <- lm(price_gr ~ .,
             data = entrena %>% select(-property_type))

summary(modelo)

entrena %>% str

### Predicciones y error en log
df <- data.frame(pred = predict(modelo, prueba), 
                 obs = prueba$price,
                 country = prueba$country) %>% 
  mutate(residuo = obs - pred)

sum((df$obs - df$pred)^2) / nrow(df)
cor(df$obs, df$pred)^2

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
df.exp <- data.frame(pred = exp(predict(modelo, prueba)), 
                     obs = exp(prueba$price),
                     country = prueba$country) %>% 
  mutate(residuo = obs - pred)

sqrt(sum((df.exp$obs - df.exp$pred)^2) / nrow(df.exp))

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
