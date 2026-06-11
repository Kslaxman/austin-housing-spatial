# Title: Spatial Weights
# File: R/04_spatial_autocorrelation/01_spatial_weights.R
# Project: Real Estate Price Prediction with Spatial Autocorrelation
# Description: Constructs queen/rook contiguity neighbors and row-standardized weights; reports
# connectivity disgnostics

library(tidyverse)
library(sf)
library(spdep)
library(tmap)

housing <- readRDS("data/processed/housing_final.rds")

## Queen contiguity neighbors list
nb_queen <- poly2nb(housing, queen = TRUE)

summary(nb_queen)

## Disconnected tracts
no_neighbors <- sum(card(nb_queen) == 0)
cat("\nTracts with zero neighbors: ", no_neighbors, "\n")

if(no_neighbors > 0) {
  island_ids <- which(card(nb_queen) == 0)
  cat("Island tract row indices: ", island_ids, "\n")
} else {
  cat("All tracts are connected\n")
}

## Spatial weight list (row-standardized)
weights_queen <- nb2listw(nb_queen, style = "W", zero.policy = TRUE)
cat("N tracts: ", length(weights_queen$neighbours), "\n")

## Neighbor connection map
tract_coords <- housing %>% st_centroid() %>% st_coordinates()

nb_lines <- nb2lines(nb_queen, coords = tract_coords, as_sf = TRUE) %>% st_set_crs(st_crs(housing))

housing <- housing %>% mutate(n_neighbors = card(nb_queen))

tmap_mode("plot")

connectivity_map <- tm_shape(housing) + 
  tm_fill("n_neighbors", 
          fill.scale = tm_scale_intervals(style = "quantile", values = "Blues", value.na = NA), 
          fill.legend = tm_legend(title = "Number of Neighbors")) + 
  tm_borders(col = "grey30", lwd = 0.3) +
  tm_shape(nb_lines) + 
  tm_lines(col = "#D7191C", lwd = 0.8, col_alpha = 0.7) + 
  tm_title("Queen Contiguity - Neighbor Connections", 
           position = tm_pos_out("center", "top"),
           size = 1.1, 
           fontface = "bold") +
  tm_layout(frame = FALSE, 
            legend.outside = TRUE) + 
  tm_credits("Red lines connect neighboring census tracts", 
             position = tm_pos_in("left", "bottom"), 
             size = 0.6)

print(connectivity_map)

print(connectivity_map)

saveRDS(nb_queen, "data/processed/nb_queen.rds")
saveRDS(weights_queen, "data/processed/weights_queen.rds")