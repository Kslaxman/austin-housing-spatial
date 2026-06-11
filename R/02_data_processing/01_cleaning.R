# Title: Cleaning
# File: R/02_data_processing/01_cleaning.R
# Project: Real Estate Price Prediction with Spatial Autocorrelation
# Description: Cleans the merged data and engineers modeling features (percentage variables, housing age, log transforms)
# handles missing values.

library(tidyverse)
library(sf)
library(janitor)

housing_joined <- readRDS("data/processed/housing_with_amenities.rds")

cat(nrow(housing_joined), "tracts, ", ncol(housing_joined), "columns")
head(housing_joined)

housing <- housing_joined %>% clean_names() %>% 
  select(
    geoid,
    tract_name = name,
    home_value = median_home_value_e,
    income = median_income_e,
    rent = median_gross_rent_e,
    bachelors = above_bachelors_e,
    masters = above_masters_e,
    rooms = median_rooms_e,
    year_built = median_year_built_e,
    vacant = vacant_units_e,
    total_hh = total_housing_e,
    population = total_pop_e,
    med_age = median_age_e,
    nh_white = nh_white_e,
    nh_black = nh_black_e,
    hispanic = hispanic_e,
    owner_occ = owner_occupied_e,
    renter_occ = renter_occupied_e,
    n_schools,
    n_parks,
    n_transit,
    n_hospitals,
    dist_school_m,
    dist_park_m,
    dist_transit_m,
    dist_hospital_m,
    geometry
  )

housing <- housing %>% mutate(
  bachelors_pct = (bachelors / population) * 100,
  masters_pct = (masters / population) * 100,
  white_pct = (nh_white / population) * 100,
  black_pct = (nh_black / population) * 100,
  hispanic_pct = (hispanic / population) * 100,
  renter_pct = (renter_occ / total_hh) * 100,
  vacant_pct = (vacant / total_hh) * 100,
  housing_age = 2025 - year_built,
  log_home_value = log(home_value),
  log_income = log(income),
  log_dist_school = log(dist_school_m + 1),
  log_dist_park = log(dist_park_m + 1),
  log_dist_transit = log(dist_transit_m + 1),
  log_dist_hospital = log(dist_hospital_m + 1)
  
)

missing <- housing %>% st_drop_geometry() %>% summarise(across(everything(), ~ sum(is.na(.)))) %>%
            pivot_longer(everything(), names_to = "variable", values_to = "n_missing") %>%
            filter(n_missing > 0) %>% arrange(desc(n_missing))
print(nrow(missing))

## Filter out non-residential tracts
n_before <- nrow(housing)

housing_clean <- housing %>%
                  filter(!is.na(home_value), home_value > 0, !is.na(income), !is.na(population), population > 100)
housing_clean <- housing_clean %>% mutate(across(c(rent, year_built, housing_age), ~replace_na(., median(., na.rm = TRUE))))

n_after <- nrow(housing_clean)
rows_removed <- n_before - n_after
print(rows_removed)

housing_clean %>% st_drop_geometry() %>% summarise(
  n_tracts = n(), min_value = scales::dollar(min(home_value, na.rm = TRUE)),
  median_value = scales::dollar(median(home_value, na.rm = TRUE)),
  max_value = scales::dollar(max(home_value, na.rm = TRUE)),
  avg_schools = round(mean(n_schools), 1),
  avg_parks = round(mean(n_parks), 1),
  avg_transit = round(mean(n_transit), 1),
  avg_hospitals = round(mean(n_hospitals), 1)
) %>% glimpse()

saveRDS(housing_clean, "data/processed/housing_final.rds")

st_write(housing_clean, "data/processed/housing_final.gpkg", delete_dsn = TRUE, quiet = TRUE)
