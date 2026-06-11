# Title: Pull Amenities
# File: R/01_data_collection/02_pull_amenities.R
# Project: Real Estate Price Prediction with Spatial Autocorrelation
# Description: Retrieves the four amenity datasets (NCES schools, Austin, parks, 
# capmetro, transit, DataLumos hospitals), clips to the study area.

library(tidyverse)
library(sf)
library(tigris)
library(tidycensus)
library(tmap)
library(gtfstools)
library(osmdata)

options(tigris_use_cache = TRUE)

#####
# Travis county boundary
#####

travis <- counties(state = "TX", cb = TRUE, year = 2025) %>% filter(NAME == "Travis") %>%
            st_transform(4326)


## Pull schools (https://nces.ed.gov/programs/edge/Geographic/SchoolLocations)
schools_raw <- st_read("data/raw/Shapefile_SCH/EDGE_GEOCODE_PUBLICSCH_2425.shp", quiet = TRUE)
names(schools_raw)
schools <- schools_raw %>% st_transform(4326) %>% st_intersection(travis) %>% select(school_id = NCESSCH, name = NAME, geometry) %>%
            filter(!is.na(geometry)) %>% mutate(amenity_type = "school")

cat("Schools Found:", nrow(schools), "\n")


## Pull parks (https://data.austintexas.gov/Recreation-and-Culture/BOUNDARIES_city_of_austin_parks/v8hw-gz65/about_data)
parks_raw <- st_read("data/raw/Shapefile_SCH/BOUNDARIES_city_of_austin_parks_20260520/geo_export_c316ffe3-b28a-4927-975c-83d445e24576.shp", quiet = TRUE)
names(parks_raw)
parks <- parks_raw %>% st_transform(4326) %>% st_intersection(travis) %>% select(park_id = objectid, name = location_n, geometry) %>% 
          filter(!is.na(geometry)) %>% st_centroid() %>% mutate(amenity_type = "park")

cat("Parks found:", nrow(parks), "\n")

## Pull transit stops (https://catalog.data.gov/dataset/capmetro-gtfs)
transit_raw <- read_gtfs("data/raw/Shapefile_SCH/capmetro.zip")
transit <- transit_raw$stops %>% st_as_sf(coords = c("stop_lon", "stop_lat"), crs = 4326) %>% st_intersection(travis) %>% 
            select(stop_id, stop_name, geometry) %>% filter(!is.na(geometry)) %>% mutate(amenity_type = "transit")

cat("Transit Stops Found: ", nrow(transit), "\n")

## Pull hospitals (https://www.datalumos.org/datalumos/project/239108/version/V1/view)
hospitals_raw <- st_read("data/raw/Shapefile_SCH/hospitals-3-shapefile/Hospitals.shp", quiet = TRUE)
names(hospitals_raw)
hospitals <- hospitals_raw %>% st_transform(4326) %>% st_intersection(travis) %>% select(hospital_id = ID, name = NAME, geometry) %>% filter(!is.na(geometry)) %>% 
              mutate(amenity_type = "hospital")

cat("Hospitals Found: ", nrow(hospitals), "\n")

## Save layers

st_write(schools, "data/output/schools.gpkg", delete_dsn = TRUE)
st_write(parks, "data/output/parks.gpkg", delete_dsn = TRUE)
st_write(transit, "data/output/transit.gpkg", delete_dsn = TRUE)
st_write(hospitals, "data/output/hospitals.gpkg", delete_dsn = TRUE)

