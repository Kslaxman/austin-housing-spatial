# Title: Spatial Join
# File: R/01_data_collection/03_spatial_join.R
# Project: Real Estate Price Prediction with Spatial Autocorrelation
# Description: Reprojects layers to UTM 14N and spatially joins amenities to tracts, engineering
# per-tract counts and nearest-distance features.

library(tidyverse)
library(sf)
library(tidycensus)

housing_raw <- readRDS("data/raw/housing_raw.rds")
schools <- st_read("data/output/schools.gpkg", quiet = TRUE)
parks <- st_read("data/output/parks.gpkg", quiet = TRUE)
transit <- st_read("data/output/transit.gpkg", quiet = TRUE)
hospitals <- st_read("data/output/hospitals.gpkg", quiet = TRUE)

cat("Housing tracts: ", nrow(housing_raw), "\n")
cat("Schools: ", nrow(schools), "\n")
cat("Parks: ", nrow(parks), "\n")
cat("Transit stops: ", nrow(transit), "\n")
cat("Hospitals: ", nrow(hospitals), "\n")

## Align coordinate reference system
cat("Housing CRS: ", st_crs(housing_raw)$input, "\n")
cat("Schools CRS: ", st_crs(schools)$input, "\n")

housing_proj <- st_transform(housing_raw, crs = 32614)
schools_proj <- st_transform(schools, crs = 32614)
parks_proj <- st_transform(parks, crs = 32614)
transit_proj <- st_transform(transit, crs = 32614)
hospitals_proj <- st_transform(hospitals, crs = 32614)

geo_crs_match <- all(st_crs(housing_proj) == st_crs(schools_proj), st_crs(housing_proj) == st_crs(parks_proj),
                     st_crs(housing_proj) == st_crs(transit_proj), st_crs(housing_proj) == st_crs(hospitals_proj))

cat("CRS alignment check: ", ifelse(geo_crs_match, "All match", "Mismatch"))

## Count amenities within each census tract
count_amenities_in_tract <- function(tracts, points, col_name) {
  joined <- st_join(tracts["GEOID"], points, join = st_contains)
  counts <- joined %>% st_drop_geometry() %>% group_by(GEOID) %>% summarise(!!col_name := sum(!is.na(amenity_type)), .groups = "drop")
  return(counts)
}

school_counts <- count_amenities_in_tract(housing_proj, schools_proj, "n_schools")
park_counts <- count_amenities_in_tract(housing_proj, parks_proj, "n_parks")
transit_counts <- count_amenities_in_tract(housing_proj, transit_proj, "n_transit")
hospital_counts <- count_amenities_in_tract(housing_proj, hospitals_proj, "n_hospitals")

cat("Tracts with at least 1 school: ", sum(school_counts$n_schools > 0), "\n")
cat("Tracts with at least 1 park: ", sum(park_counts$n_parks > 0), "\n")
cat("Tracts with at least 1 transit: ", sum(transit_counts$n_transit > 0), "\n")
cat("Tracts with at least 1 hospital: ", sum(hospital_counts$n_hospitals > 0), "\n")

## Distance to nearest amenity (meters)
tract_centroids <- st_centroid(housing_proj)
dist_to_nearest <- function(centroids, points, col_name) {
  nearest_idx <- st_nearest_feature(centroids, points)
  
  distances <- st_distance(centroids, points[nearest_idx, ], by_element = TRUE)
  
  tibble(GEOID = centroids$GEOID, !!col_name := as.numeric(distances))
}

dist_school <- dist_to_nearest(tract_centroids, schools_proj, "dist_school_m")
dist_park <- dist_to_nearest(tract_centroids, parks_proj, "dist_park_m")
dist_transit <- dist_to_nearest(tract_centroids, transit_proj, "dist_transit_m")
dist_hospital <- dist_to_nearest(tract_centroids, hospitals_proj, "dist_hospital_m")

cat("\n Nearest school — min:", round(min(dist_school$dist_school_m)),
    "m | max:", round(max(dist_school$dist_school_m)), "m\n")
cat(" Nearest park — min:", round(min(dist_park$dist_park_m)),
    "m | max:", round(max(dist_park$dist_park_m)), "m\n")
cat(" Nearest transit  — min:", round(min(dist_transit$dist_transit_m)),
    "m | max:", round(max(dist_transit$dist_transit_m)), "m\n")
cat(" Nearest hospital — min:", round(min(dist_hospital$dist_hospital_m)),
    "m | max:", round(max(dist_hospital$dist_hospital_m)), "m\n")


housing_with_amenities <- housing_proj %>%
                            left_join(school_counts, by = "GEOID") %>%
                            left_join(park_counts, by = "GEOID") %>%
                            left_join(transit_counts, by = "GEOID") %>%
                            left_join(hospital_counts, by = "GEOID") %>%
                            left_join(dist_school, by = "GEOID") %>%
                            left_join(dist_park, by = "GEOID") %>%
                            left_join(dist_transit, by = "GEOID") %>%
                            left_join(dist_hospital, by = "GEOID") %>%
                            mutate(across(starts_with("n_"), ~ replace_na(.x, 0)))

cat("Rows (tracts): ", nrow(housing_with_amenities), "\n")
cat("Columns: ", ncol(housing_with_amenities), "\n")

saveRDS(housing_with_amenities, "data/processed/housing_with_amenities.rds")
