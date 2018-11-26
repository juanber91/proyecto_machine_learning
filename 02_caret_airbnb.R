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

<<<<<<< Updated upstream
gbmFit1 <- train(price ~ ., data = entrena, 
                 method = "gbm", 
=======
gbmFit1 <- train(price ~ ., data = entrena,
                 method = "gbm",
>>>>>>> Stashed changes
                 trControl = fitControl,
                 ## This last option is actually one
                 ## for gbm() that passes through
                 verbose = FALSE)

<<<<<<< Updated upstream
=======
beep()
>>>>>>> Stashed changes
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

<<<<<<< Updated upstream
mod_boosting$valid.error
=======
mod_boosting$valid.error %>% tail
>>>>>>> Stashed changes
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
<<<<<<< Updated upstream
  geom_point(aes(color = country)) +
=======
  geom_point(aes(color = country), size = 0.1) +
>>>>>>> Stashed changes
  geom_abline() +
  coord_equal() +
  scale_x_continuous(labels = scales::comma) +
  scale_y_continuous(labels = scales::comma) +
  theme_minimal()

<<<<<<< Updated upstream
=======
df %>% 
  ggplot(aes(residuo)) +
  geom_histogram(aes(y = ..density..), fill = 'royal blue') +
  geom_density(color = 'red', size = 1, fill = 'red', alpha = 0.2) +
  scale_x_continuous(labels = scales::comma) +
  scale_y_continuous(labels = scales::comma) +
  theme_minimal()

>>>>>>> Stashed changes
mod_boosting$train.error