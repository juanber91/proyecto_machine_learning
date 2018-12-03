options(java.parameters = "-Xmx8g")
library(tidyverse)
library(data.table)
library(magrittr)
library(corrplot)

# Cargamos datos y limpiamos ----------------------------------------------
archivos <- list.files('datos/EUA/', pattern = 'csv', recursive = F)

# archivos <- 'NY.csv'

listings_raw <- map_df(archivos, ~fread(paste0('datos/EUA/', .),
                                        colClasses = c('id' = 'text',
                                                       'zipcode' = 'text'),
                                        encoding = 'UTF-8'), .id = 'file') %>% 
  bind_rows

listings_raw %<>% 
  select(-c(contains('url'), description, scrape_id, name, summary, space, 
            neighborhood_overview, notes,
            transit, access, interaction, house_rules, host_name,
            host_location, host_about,
            host_neighbourhood, neighbourhood, neighbourhood_cleansed, #city,
            state, 
            market:country_code, amenities, host_verifications,
            requires_license,
            host_acceptance_rate, host_listings_count, square_feet,
            jurisdiction_names, last_scraped, host_has_profile_pic,
            street, neighbourhood_group_cleansed, license, calendar_updated,
            host_since, calendar_last_scraped, first_review, last_review,
            experiences_offered, has_availability,
            is_business_travel_ready)) %>% 
  filter(property_type %in% c('Hostel', 'Bed and breakfast', 'Loft',
                              'Condominium', 'House', 'Apartment')) %>% 
  mutate_at(vars(contains('price'), security_deposit, cleaning_fee,
                 host_response_rate, extra_people),
            funs(parse_number)) %>% 
  mutate(zipcode = as.numeric(zipcode)) %>% 
  mutate_at(vars(id, review_scores_rating, accommodates, bathrooms, bedrooms,
                 beds, guests_included, reviews_per_month), 
            funs(as.numeric)) %>% 
  dplyr::filter(country != 'Switzerland') 


listing_mod <- listings_raw %>% 
  dplyr::filter(property_type %in% c('Hostel', 'Bed and breakfast', 'Loft',
                                     'Condominium', 'House', 'Apartment'),
                accommodates <= 6,
                beds > 0, beds <= 4,
                bedrooms > 0, bedrooms <= 4,
                bathrooms > 0, bathrooms <= 2,
                guests_included <= 4,
                reviews_per_month <= 4,
                cleaning_fee <= price,
                # price > 20, price <= 250) %>% 
                price > 39, price <= 375) %>% 
  select(id, file, price, review_scores_rating, room_type, property_type,
         accommodates, bathrooms, beds, guests_included, bed_type,
         reviews_per_month, cleaning_fee,
         bedrooms, latitude, longitude) %>% 
  na.omit() %>% 
  mutate(cleaning_fee = parse_number(cleaning_fee),
         cleaning_fee = ifelse(is.na(cleaning_fee)|cleaning_fee==0, 0, 1),
         bed_type = ifelse(bed_type != 'Real Bed', 'Other', bed_type),
         room_type = ifelse(room_type != 'Entire home/apt', 'Private/shared',
                            room_type)) %>% 
  mutate_if(is.character, funs(as.factor)) %>% 
  mutate(longitude = round(as.numeric(as.character(longitude)), 2),
         latitude = round(as.numeric(as.character(latitude)), 2)) %>% 
  mutate_at(vars(bathrooms, beds, bedrooms, guests_included, cleaning_fee),
            funs(as.factor)) %>% 
  mutate(longitude = abs(longitude)) %>% 
  mutate_if(is.numeric, funs(log))

# listin_log <- listing_mod %>% 
#   mutate_if(is.numeric, funs(log))
# mutate(price_log = log(price))

# listing_mod$cleaning_fee %>% quantile(seq(0,1,0.01), na.rm = T) 

listing_mod %>% summary


# Entrenamiento y prueba --------------------------------------------------
set.seed(124458)
entrena <- sample_frac(listing_mod, 0.75)
prueba <- listing_mod %>% dplyr::filter(!id %in% entrena$id) %>% select(-id)
entrena %<>% select(-id)

save(entrena, prueba, file = 'datos/bases_airbnb.RData')


# EDA ---------------------------------------------------------------------

listing_mod %>% 
  ggplot(aes(cleaning_fee!=0, log(price))) +
  facet_wrap(~country, scales = 'free_x') +
  geom_boxplot() 

listing_mod %>% 
  ggplot(aes(cleaning_fee, price)) +
  geom_smooth(method = 'lm') +
  geom_point()
