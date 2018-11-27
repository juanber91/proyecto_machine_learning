library(tidyverse)
library(data.table)
library(magrittr)

<<<<<<< Updated upstream
# Cargamos datos y limpiamos ----------------------------------------------
archivos <- list.files('datos/', pattern = 'csv', recursive = F)

listings_raw <- map(archivos, ~fread(paste0('datos/', .),
                                     colClasses = 'text',
                                     encoding = 'UTF-8')) %>% 
  bind_rows

listings_raw %<>% 
  select(-c(contains('url'), description, scrape_id, name, summary, space, neighborhood_overview, notes,
            transit, access, interaction, house_rules, host_name, host_location, host_about,
            host_neighbourhood, neighbourhood, neighbourhood_cleansed, city, state, 
            market:country_code, amenities, host_verifications)) %>% 
  mutate(price = parse_number(price),
         zipcode = as.numeric(zipcode)) %>% 
  mutate_at(vars(id, review_scores_rating, accommodates, bathrooms, bedrooms,
                 beds, guests_included),
            funs(as.numeric)) %>% 
  filter(country != 'Switzerland')

listing_mod <- listings_raw %>% 
  select(id, country, price, review_scores_rating, room_type,
         accommodates, bathrooms, beds, guests_included,
           bedrooms, neighbourhood_group_cleansed, latitude, longitude) %>% 
  na.omit() %>% 
  mutate_if(is.character, funs(as.factor)) %>% 
  filter(price > 0, price <= 250) %>% 
  mutate(longitude = round(as.numeric(as.character(longitude)), 3),
         latitude = round(as.numeric(as.character(latitude)), 3)) %>% 
  mutate_if(is.numeric, funs(log1))

# listing_mod %>% str
# data.frame(colSums(is.na(listing_mod)))
# 
# set.seed(815)
# listing_mod$price %>% quantile(seq(0, 1, 0.01))


# Entrenamiento y prueba --------------------------------------------------
entrena <- sample_frac(listing_mod, 0.75)
prueba <- listing_mod %>% filter(!id %in% entrena$id) %>% select(-id)
entrena %<>% select(-id)

save(entrena, prueba, file = 'datos/bases_airbnb.RData')

# Modelo lineal -----------------------------------------------------------
modelo <- lm(price ~ country + review_scores_rating + accommodates + bathrooms + 
               bedrooms + room_type + latitude:longitude +
=======

# Modelo lineal -----------------------------------------------------------
modelo <- lm(price ~ country + review_scores_rating + accommodates + bathrooms + 
               bedrooms + room_type + property_type + bed_type +
               latitude:longitude + reviews_per_month +
>>>>>>> Stashed changes
               beds + guests_included,
             data = entrena)

summary(modelo)


# Boosting ----------------------------------------------------------------
library(gbm)
mod_boosting <- gbm(price ~ country + review_scores_rating + accommodates + bathrooms + 
<<<<<<< Updated upstream
                      bedrooms + room_type + latitude:longitude +
=======
                      bedrooms + room_type + property_type + bed_type +
                      latitude:longitude + reviews_per_month +
>>>>>>> Stashed changes
                      beds + guests_included,
                    data = entrena,
                    distribution = 'laplace',
                    n.trees = 400, 
                    interaction.depth = 3,
                    shrinkage = 1, # tasa de aprendizaje
                    bag.fraction = 1,
                    train.fraction = 0.75)

mod_boosting
summary(mod_boosting)

# df <- data.frame(pred = predict(mod_boosting, prueba), obs = log(prueba$price))
<<<<<<< Updated upstream
df <- data.frame(pred = exp(predict(mod_boosting, prueba)), 
                 obs = exp(prueba$price),
=======
df <- data.frame(pred = predict(mod_boosting, prueba), 
                 obs = prueba$price,
>>>>>>> Stashed changes
                 country = prueba$country)

df %>% 
ggplot(aes(obs, pred)) +
  geom_point(aes(color = country)) +
  geom_abline() +
  coord_equal() +
  scale_x_continuous(labels = scales::comma) +
  scale_y_continuous(labels = scales::comma) +
  theme_minimal()

<<<<<<< Updated upstream
mod_boosting$train.error
=======
mod_boosting$valid.error
>>>>>>> Stashed changes
