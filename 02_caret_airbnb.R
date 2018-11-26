library(tidyverse)
library(data.table)
library(magrittr)
library(caret)
library(beepr)

load('datos/bases_airbnb.RData')

fitControl <- trainControl(## 10-fold CV
  method = "repeatedcv",
  number = 10,
  ## repeated ten times
  repeats = 10)

gbmFit1 <- train(price ~ ., data = entrena, 
                 method = "gbm", 
                 trControl = fitControl,
                 ## This last option is actually one
                 ## for gbm() that passes through
                 verbose = FALSE)

gbmFit1$bestTune

library(gbm)
mod_boosting <- gbm(price ~ .,
                    data = entrena,
                    n.trees = 150, 
                    interaction.depth = 3,
                    shrinkage = 0.1, # tasa de aprendizaje
                    n.minobsinnode = 10,
                    bag.fraction = 1,
                    train.fraction = 0.75)

mod_boosting$valid.error
summary(mod_boosting)


# df <- data.frame(pred = predict(mod_boosting, prueba), obs = log(prueba$price))
df <- data.frame(pred = predict(mod_boosting, prueba), 
                 obs = prueba$price,
                 country = prueba$country) %>% 
  mutate(residuo = obs - pred)

qqnorm(df$residuo)
qqline(df$residuo)

df %>% 
  ggplot(aes(obs, pred)) +
  geom_point(aes(color = country)) +
  geom_abline() +
  coord_equal() +
  scale_x_continuous(labels = scales::comma) +
  scale_y_continuous(labels = scales::comma) +
  theme_minimal()

mod_boosting$train.error