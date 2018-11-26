library(tidyverse)
library(data.table)
library(magrittr)

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
                 beds, guests_included, reviews_per_month), 
            funs(as.numeric)) %>% 
  dplyr::filter(country != 'Switzerland') 


listing_mod <- listings_raw %>% 
  dplyr::filter(property_type %in% c('Hostel', 'Bed and breakfast', 'Loft',
                                     'Condominium', 'House', 'Apartment'),
                bed_type != 'Airbed',
                beds > 0, beds <= 10,
                bedrooms > 0, bedrooms <= 6,
                bathrooms > 0,bathrooms <= 5,
                price > 0, price <= 250) %>% 
  select(id, country, price, review_scores_rating, room_type, property_type,
         accommodates, bathrooms, beds, guests_included, bed_type,
         reviews_per_month,
         bedrooms, neighbourhood_group_cleansed, latitude, longitude) %>% 
  na.omit() %>% 
  mutate(bed_type = ifelse(bed_type != 'Real Bed', 'Other', bed_type),
         room_type = ifelse(room_type != 'Entire home/apt', 'Private/shared', room_type)) %>% 
  mutate_if(is.character, funs(as.factor)) %>% 
  mutate(longitude = round(as.numeric(as.character(longitude)), 3),
         latitude = round(as.numeric(as.character(latitude)), 3)) %>% 
  mutate_if(is.numeric, funs(log)) 

listing_mod$property_type %>% unique

# listing_mod %>% str
# data.frame(colSums(is.na(listing_mod)))
# 
# set.seed(815)
# listing_mod$price %>% quantile(seq(0, 1, 0.01))


# Entrenamiento y prueba --------------------------------------------------
set.seed(124458)
entrena <- sample_frac(listing_mod, 0.75)
prueba <- listing_mod %>% dplyr::filter(!id %in% entrena$id) %>% select(-id)
entrena %<>% select(-id)

save(entrena, prueba, file = 'datos/bases_airbnb.RData')
