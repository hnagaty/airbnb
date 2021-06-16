#' Clustering of destination countries
#' 

library(tidyverse)


dataPath <- "/home/hnagaty/dataNAS/airbnb/"
dataPath <- '~/data/'

countries <- read_csv(paste0(dataPath, "countries.csv"),
                      col_types = cols(
                        country_destination = col_factor(levels = dest_levels,ordered = FALSE, include_na = FALSE),
                        lat_destination = col_double(),
                        lng_destination = col_double(),
                        distance_km = col_double(),
                        destination_km2 = col_double(),
                        destination_language = col_factor(levels = NULL, ordered = FALSE, include_na = FALSE),
                        language_levenshtein_distance = col_double()))
glimpse(countries)
summary(countries)
countries

ggplot(countries, aes(lng_destination, lat_destination, col = language_levenshtein_distance)) +
  geom_text(aes(label=country_destination), alpha = 0.8, col = 'red') +
  geom_point(aes(size = destination_km2), alpha = 0.7)

destination_clusters <- tibble(old = c("NDF", "US", "other", "FR", "CA", "GB", "ES", "IT", "PT", "NL", "DE","AU"),
                               new = c('NDF', 'US', 'other', 'EUR', 'AUCA', 'GB', 'EUR', 'EUR', 'EUR', 'EUR', 'EUR', 'AUCA'))
