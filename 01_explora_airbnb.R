library(tidyverse)
library(data.table)
library(magrittr)

# Cargamos datos y limpiamos ----------------------------------------------
listings_raw <- fread('datos/listings.csv', encoding = 'UTF-8') %>% 
  select(-c(contains('url'), description, scrape_id, name, summary, space, neighborhood_overview, notes,
            transit, access, interaction, house_rules, host_name, host_location, host_about,
            host_neighbourhood, neighbourhood, neighbourhood_cleansed, city, state, 
            market:country, amenities, host_verifications)) %>% 
  mutate(price = parse_number(price),
         zipcode = as.numeric(zipcode))

listings_raw %>% head

listing_mod <- listings_raw %>% 
  select(id, price, review_scores_rating, room_type, accommodates, bathrooms, 
           bedrooms, neighbourhood_group_cleansed, zipcode) %>% 
  na.omit() %>% 
  mutate_if(is.character, funs(as.factor)) %>% 
  filter(price > 0, price < 250)

listing_mod %>% str
data.frame(colSums(is.na(listing_mod)))

listing_mod$price %>% quantile(seq(0, 1, 0.01))


# Entrenamiento y prueba --------------------------------------------------
entrena <- sample_frac(listing_mod, 0.75)
prueba <- listing_mod %>% filter(!id %in% entrena$id) %>% select(-id)
entrena %<>% select(-id)

modelo <- lm(price ~ review_scores_rating + accommodates + bathrooms + 
               bedrooms + neighbourhood_group_cleansed + room_type,
             data = entrena)

summary(modelo)


# Boosting ----------------------------------------------------------------
library(gbm)
mod_boosting <- gbm(log(price) ~.,  data = entrena,
                    distribution = 'laplace',
                    n.trees = 200, 
                    interaction.depth = 3,
                    shrinkage = 1, # tasa de aprendizaje
                    bag.fraction = 1,
                    train.fraction = 0.75)

mod_boosting
summary(mod_boosting)

df <- data.frame(pred = predict(mod_boosting, prueba), obs = log(prueba$price))

df %>% 
ggplot(aes(obs, pred)) +
  geom_point() +
  geom_abline() +
  coord_equal() +
  scale_x_continuous(labels = scales::comma) +
  scale_y_continuous(labels = scales::comma) +
  theme_minimal()

