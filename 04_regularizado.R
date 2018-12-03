library(tidyverse)
library(data.table)
library(magrittr)
library(caret)
library(beepr)

load('datos/bases_airbnb.RData')

library(glmnet)

entrena <- sample_frac(listings_raw, 0.75)
prueba <- listing_mod %>% dplyr::filter(!id %in% entrena$id) %>% select(-id)
entrena %<>% select(-id)

x <- model.matrix(~., entrena[,-19])
x %>% dim
y <- entrena$price
y %>% length

cctrlR <- trainControl(method = "cv", number = 3, 
                       returnResamp = "all", search = "random")

test_class_cv_model <- train(x, y, 
                             method = "glmnet", 
                             trControl = cctrlR,
                             metric = "RMSE",
                             tuneGrid = expand.grid(.alpha = seq(0, 1, 0.05),
                                                    .lambda = exp(seq(-5,5,1))))

test_class_cv_model$bestTune
plot(test_class_cv_model)

cv_mod_ridge <- cv.glmnet(x, y,
                          alpha = 1,
                          family = 'gaussian',
                          intercept = T, nfolds = 10,
                          nlambda = 50)
plot(cv_mod_ridge)

cv_mod_ridge$lambda.1se

mod_ridge <- glmnet(x, y, 
                    alpha = 1, #ridge
                    family = 'gaussian', 
                    intercept = T, 
                    lambda = 0.0002112753)
                    # nlambda = 50) #normalmente ponemos intercept = T


x.test <- model.matrix(~., prueba[,-2])
x.test %>% dim
y.test <- prueba$price
y.test %>% length

df.reg <- data.frame(pred = predict(mod_ridge, x.test),
# df.reg <- data.frame(pred = mod_ridge$beta, x.test),
                 obs = prueba$price,
                 country = prueba$country) %>% 
  mutate(residuo = obs - s0)

sqrt(sum((df.reg$obs - df.reg$s0)^2) / nrow(df.reg))

cor(df.reg$obs, df.reg$s0)^2

qqnorm(df.reg$residuo)
qqline(df.reg$residuo)

df.reg %>% 
  ggplot(aes(obs, s0)) +
  geom_point(aes(color = country), size = 0.1) +
  geom_abline() +
  coord_equal() +
  scale_x_continuous(labels = scales::comma) +
  scale_y_continuous(labels = scales::comma) +
  theme_minimal()


df.reg %>% 
  ggplot(aes(residuo)) +
  geom_density(color = 'red', size = 1, fill = 'red', alpha = 0.2) +
  geom_histogram(aes(y = ..density..), fill = 'royal blue', alpha = 0.75) +
  scale_x_continuous(labels = scales::comma) +
  # scale_y_continuous(labels = scales::comma) +
  theme_minimal()


### Predicciones y error en exp
df.exp <- data.frame(pred = exp(predict(mod_ridge, x.test)), 
                     obs = exp(prueba$price),
                     country = prueba$country) %>% 
  mutate(residuo = obs - s0)

sqrt(sum((df.exp$obs - df.exp$s0)^2) / nrow(df.exp))

qqnorm(df.exp$residuo)
qqline(df.exp$residuo)

df.exp %>% 
  ggplot(aes(obs, s0)) +
  geom_point(aes(color = country), size = 0.1) +
  geom_abline() +
  # coord_equal() +
  scale_x_continuous(labels = scales::comma) +
  scale_y_continuous(labels = scales::comma) +
  theme_minimal()


df.exp %>% 
  ggplot(aes(residuo)) +
  geom_density(color = 'red', size = 1, fill = 'red', alpha = 0.2) +
  geom_histogram(aes(y = ..density..), fill = 'royal blue', alpha = 0.75) +
  scale_x_continuous(labels = scales::comma) +
  theme_minimal()
