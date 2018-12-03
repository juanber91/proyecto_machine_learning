library(tidyverse)
library(data.table)
library(magrittr)
library(caret)
library(beepr)

load('datos/bases_airbnb.RData')

fitControl <- trainControl(method = "repeatedcv", number = 10, repeats = 10)

gbmFit1 <- train(price_gr ~ ., data = entrena %>% select(-price),
                 method = "gbm",
                 trControl = fitControl,
                 verbose = FALSE)


gbmFit1$bestTune



library(gbm)
mod_boosting <- gbm(price_gr ~ .,
                    data = entrena %>% select(-price),
                    n.trees = 150, 
                    interaction.depth = 3,
                    shrinkage = 0.1,
                    n.minobsinnode = 10,
                    bag.fraction = 1,
                    train.fraction = 0.75)

mod_boosting$valid.error %>% tail
mod_boosting$train.error %>% tail
summary(mod_boosting)


### Predicciones y error en log
df.gbm <- data.frame(pred = predict(mod_boosting, prueba, type = 'response'),
                     obs = prueba$price_gr) %>% 
  # mutate(num = apply(., 1, which.max)) %>% 
  # mutate(pred = factor(ifelse(num == 1, 'low',
  #                             ifelse(num == 2, 'medium', 'high')),
  #                      levels = c('low', 'medium', 'high')))
 mutate(pred1 = ifelse(pred > 0.5, 1, 0))

sum(df.gbm$obs == df.gbm$pred1) / nrow(df.gbm)
round(100 * prop.table(table(df.gbm$obs, df.gbm$pred1), margin = 1),1)



