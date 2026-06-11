# Title: Pull Census Data
# File: R/01_data_collection/01_pull_census_data.R
# Project: Real Estate Price Prediction with Spatial Autocorrelation
# Description: Downloads ACS 5-year estimates for Travis County tracts (home value, income, 
# education, tenure, vacancy, age, race) with tract geometry.

library(tidyverse)
library(tidycensus)
library(sf)
library(janitor)
library(httr)
library(jsonlite)
library(dplyr)
library(tibble)

get_acs_variables <- function(year = 2021, survey = "acs5", key) {
  url <- paste0("https://api.census.gov/data/",
                year,
                "/acs/",
                survey,
                "/variables.json?key=",
                key)
  raw <- fromJSON(content(GET(url), "text"))
  
  raw$variables %>% enframe(name = "variable", value = "meta") %>% mutate(
    label = sapply(meta, function(x) x$label),
    concept = sapply(meta, function(x) x$concept)
  ) %>% 
    select(variable, label, concept)
}

acs_vars <- get_acs_variables(year = 2021, survey = "acs5", key = "5ab08b7c548929377ba642a835d8181cc01b59fa")
cat("Total ACS variables available: ", nrow(acs_vars), "\n")

# Home value
acs_vars %>% filter(str_detect(label, regex('MEDIAN VALUE', ignore_case = TRUE))) %>% 
              select(variable, label, concept) %>%
              print(n = 10)

# Income
acs_vars %>% filter(str_detect(concept, regex('MEDIAN HOUSEHOLD INCOME', ignore_case = TRUE)), str_detect(label, "Estimate!!Median")) %>%
              select(variable, label, concept) %>%
              print(n = 5)


housing_raw <- get_acs(
  geography = "tract",
  state = "TX",
  county = "Travis",
  year = 2021,
  survey = "acs5",
  variables = c(
    # Outcome
    median_home_value = "B25077_001",   # Median Home Value ($)
    
    median_income = "B19013_001",       # Median Household Income
    median_gross_rent = "B25064_001",   # Median Gross Rent
    unemployed_pct = "B23025_005",      # Unemployed Percentage
    above_bachelors = "B15003_022",     # Bachelor's Degree Holders
    above_masters = "B15003_023",       # Master's Degree Holders
    median_rooms = "B25018_001",        # Median Number of Rooms
    median_year_built = "B25035_001",   # Median Year Structure Built
    vacant_units = "B25002_003",        # Vacant Housing Units
    total_housing = "B25002_001",       # Total Housing Units
    total_pop = "B01003_001",           # Total Population
    median_age = "B01002_001",          # Median Age
    nh_white = "B03002_003",            # Non-Hispanic White Alone
    nh_black = "B03002_004",            # Non-Hispanic Black Alone
    hispanic = "B03002_012",            # Hispanic or Latino
    owner_occupied = "B25003_002",      # Owner-Occupied Units
    renter_occupied = "B25003_003"      # Renter-Occupied Units
  ),
  output = "wide",
  geometry = TRUE
)

glimpse(housing_raw)
cat("\nNumber of census tracts pulled: ", nrow(housing_raw), "\n")

saveRDS(housing_raw, "data/raw/housing_raw.rds")
