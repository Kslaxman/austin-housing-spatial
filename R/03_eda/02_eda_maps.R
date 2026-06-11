# Title: Exploratory data analysis maps
# File: R/03_eda/02_eda_maps.R
# Project: Real Estate Price Prediction with Spatial Autocorrelation
# Description: Builds choropleth maps: home-value distribution, four-scheme classification comparison and per predictor maps

library(tidyverse)
library(sf)
library(tmap)
library(viridis)
library(scales)

housing <- readRDS("data/processed/housing_final.rds")

tmap_mode("plot")

map_home_value <- tm_shape(housing) + tm_polygons(fill = "home_value", fill.scale = tm_scale_intervals(style = "quantile", n = 7, values = "Blues", value.na = NA), 
                                                   fill.legend = tm_legend(title = "Median Home\nValue ($)", format = list(fun = function(x) dollar(x, scale = 1/1000, suffix = "K"))), col = "grey30", lwd = 0.3, fill_alpha = 0.95) +
                    tm_title("Median Home Values by Census Tract", size = 1.3, fontface = "bold", position = tm_pos_out("center", "top")) +
                    tm_layout(legend.outside = TRUE, legend.outside.position = "right", legend.title.size = 0.9, legend.text.size = 0.75, frame = FALSE, bg.color = "white", inner.margins = c(0.08, 0.02, 0.02, 0.02)) +
                    tm_compass(type = "arrow", position = tm_pos_in("left", "bottom"), size = 1.5) +
                    tm_scalebar(position = tm_pos_in("left", "bottom"), text.size = 0.6) +
                    tm_credits("Source: US Census ACS 5-Year 2025", position = tm_pos_out("right", "bottom"), size = 0.55)

print(map_home_value)

print(map_home_value)

map_quantile <- tm_shape(housing) + tm_fill("home_value", fill.scale = tm_scale_intervals(style = "quantile", n = 5, values = "YlOrRd"), fill.legend = tm_legend(title = "Quantile Classification")) + tm_borders(col = "grey30", lwd = 0.3) +
                tm_layout(frame = FALSE, legend.outside = TRUE, legend.outside.position = "right")

map_jenks <- tm_shape(housing) + tm_fill("home_value", fill.scale = tm_scale_intervals(style = "jenks", n = 5, values = "YlOrRd"), fill.legend = tm_legend(title = "Jenks Natural Breaks")) + tm_borders(col = "grey30", lwd = 0.3) +
              tm_layout(frame = FALSE, legend.outside = TRUE, legend.outside.position = "right")

map_equal <- tm_shape(housing) + tm_fill("home_value", fill.scale = tm_scale_intervals(style = "equal", n = 5, values = "YlOrRd"), fill.legend = tm_legend(title = "Equal Interval")) + tm_borders(col = "grey30", lwd = 0.3) +
             tm_layout(frame = FALSE, legend.outside = TRUE, legend.outside.position = "right")

map_sd <- tm_shape(housing) + tm_fill("home_value", fill.scale = tm_scale_intervals(style = "sd", n = 5, values = "YlOrRd"), fill.legend = tm_legend(title = "Standard Deviation")) + tm_borders(col = "grey30", lwd = 0.3) +
          tm_layout(frame = FALSE, legend.outside = TRUE, legend.outside.position = "right")

class_comp <- tmap_arrange(map_quantile, map_jenks, map_equal, map_sd, ncol = 2)
print(class_comp)


tmap_mode("view")

interactive_map <- tm_shape(housing) + tm_fill("home_value", fill.scale = tm_scale_intervals(style = "quantile", n = 7, values = "Greens"), fill_alpha = 0.75, fill.legend = tm_legend(title = "Median Home Value"),
                                               popup.vars = c("Tract" = "tract_name", "Home Value" = "home_value", "Median Income" = "income", "% Bachelor's+" = "bachelors_pct", "% Renter" = "renter_pct", "Schools in Tract" = "n_schools", "Transit Stops" = "n_transit", "Parks" = "n_parks", "Dist. to School (m)" = "dist_school_m", "Dist. to Transit (m)" = "dist_transit_m"),
                                               popup.format = list(home_value = list(fun = scales::dollar), income = list(fun = scales::dollar), bachelors_pct = list(digits = 1), renter_pct = list(digits = 1))) +
                    tm_borders(col = "grey30", lwd = 0.3) + tm_title("Austin Housing Values") + tm_layout(frame = FALSE)

print(interactive_map)

tmap_mode("plot")


make_choropleth <- function(data, variable, title, palette = "Blues", n = 5) {
  tm_shape(data) + 
    tm_fill(variable, fill.scale = tm_scale_intervals(style = "quantile", n = n, values = palette), fill.legend = tm_legend(title = title, text.size = 0.6, title.size = 0.8)) + 
    tm_borders(col = "grey30", lwd = 0.3) +
    tm_layout(frame = FALSE, legend.outside = TRUE, legend.outside.position = "right")
}

# The Maps
m1 <- make_choropleth(housing, "income", "Median Income", "Greens")
m2 <- make_choropleth(housing, "bachelors_pct", "% Bachelor's+", "Purples")
m3 <- make_choropleth(housing, "renter_pct", "% Renter", "Oranges")
m4 <- make_choropleth(housing, "vacant_pct", "% Vacant Units", "Reds")
m5 <- make_choropleth(housing, "n_transit", "Transit Stops", "Blues")
m6 <- make_choropleth(housing, "housing_age", "Median Housing Age", "cividis") 

predictor_maps <- tmap_arrange(m1, m2, m3, m4, m5, m6, ncol = 3)
print(predictor_maps)


