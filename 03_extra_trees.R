options(java.parameters = "-Xmx8g")
library(tidyverse)
library(data.table)
library(magrittr)
library(caret)
library(beepr)
library(extraTrees)

load('datos/bases_airbnb.RData')

cctrlR <- trainControl(method = "cv", number = 3,
                       returnResamp = "all", search = "random")

fitControl <- trainControl(method = "repeatedcv",
                           number = 10,
                           repeats = 10)

x <- model.matrix(~., entrena[,-2])
x %>% dim
y <- entrena$price
y %>% length

gbmFit1 <- train(x, y,
                 method = "extraTrees",
                 trControl = fitControl)

beep(1)
gbmFit1
gbmFit1$bestTune


# Modelo ------------------------------------------------------------------

# entrena %<>% filter(price <= log(100)) 

mod_ET <- extraTrees(x, y, mtry = 5, numRandomCuts = 18)
beep(1)
mod_ET

df.ET <- data.frame(pred = predict(mod_ET, x), 
                 obs = entrena$price,
                 country = entrena$country) %>% 
  mutate(residuo = obs - pred)

sqrt(sum((df.ET$obs - df.ET$pred)^2) / nrow(df.ET))



# Prueba ------------------------------------------------------------------

prueba %<>% filter(price <= log(100))  

x.test <- model.matrix(~., prueba[,-2])
x.test %>% dim
y.test <- prueba$price
y.test %>% length

df.ET <- data.frame(pred = predict(mod_ET, x.test), 
                 obs = prueba$price,
                 country = prueba$country) %>% 
  mutate(residuo = obs - pred)

sqrt(sum((df.ET$obs - df.ET$pred)^2) / nrow(df.ET))

cor(df.ET$obs, df.ET$pred)^2

qqnorm(df.ET$residuo)
qqline(df.ET$residuo)

df.ET %>% 
  ggplot(aes(obs, pred)) +
  geom_point(aes(color = country), size = 0.1) +
  geom_abline() +
  coord_equal() +
  theme_minimal()


df.ET %>% 
  ggplot(aes(residuo)) +
  geom_density(color = 'red', size = 1, fill = 'red', alpha = 0.2) +
  geom_histogram(aes(y = ..density..), fill = 'royal blue', alpha = 0.75) +
  scale_x_continuous(labels = scales::comma) +
  # scale_y_continuous(labels = scales::comma) +
  theme_minimal()


### Predicciones y error en exp
df.exp <- data.frame(pred = exp(predict(mod_ET, x.test)), 
                     obs = exp(prueba$price),
                     country = prueba$country) %>% 
  mutate(residuo = obs - pred)

sqrt(sum((df.exp$obs - df.exp$pred)^2) / nrow(df.exp))

qqnorm(df.exp$residuo)
qqline(df.exp$residuo)

df.exp %>% 
  filter(residuo > -38, residuo < 64) %>%
  # filter(residuo < -38 | residuo > 64) %>%
  ggplot(aes(obs, pred)) +
  geom_point(aes(color = country), size = 0.1) +
  geom_abline() +
  coord_equal() +
  scale_x_continuous(labels = scales::comma) +
  scale_y_continuous(labels = scales::comma) +
  theme_minimal()


df.exp %>% 
  # filter(residuo > -38, residuo < 64) %>%
  ggplot(aes(residuo)) +
  geom_density(color = 'red', size = 1, fill = 'red', alpha = 0.2) +
  geom_histogram(aes(y = ..density..), fill = 'royal blue', alpha = 0.75) +
  scale_x_continuous(labels = scales::comma) +
  theme_minimal()
