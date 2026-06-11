# Title: GWR coefficient interpretation and mapping
# File: R/06_interpretation/02_gwr_interpretation.R
# Project: Real Estate Price Prediction with Spatial Autocorrelation
# Description: Interprets and maps GWR local coefficients, resolving the county-wide 
# transit paradox

library(tidyverse)
library(sf)
library(tmap)
library(patchwork)
library(ggplot2)
library(viridis)

gwr_model <- readRDS("outputs/models/gwr_model.rds")
housing <- readRDS("data/processed/housing_gwr.rds")

gwr_sf <- gwr_model$SDF %>% st_as_sf() %>% st_transform(4326)

# t-statistic
tv_cols <- names(gwr_sf)[str_detect(names(gwr_sf), "_TV$")]
cat("T-statistic columns: ")
cat(paste(" -", tv_cols, collapse = "\n"), "\n\n")

# Flag significance at 95% level (|t| > 1.96)
gwr_sf <- gwr_sf %>% mutate(income_sig = abs(log_income_TV) > 1.96,
                            edu_sig = abs(bachelors_pct_TV) > 1.96,
                            renter_sig = abs(renter_pct_TV) > 1.96,
                            transit_dist_sig = abs(log_dist_transit_TV) > 1.96,
                            income_masked = if_else(income_sig, log_income, NA_real_),
                            education_masked = if_else(edu_sig, bachelors_pct, NA_real_),
                            renter_masked = if_else(renter_sig, renter_pct, NA_real_),
                            transit_dist_masked = if_else(transit_dist_sig, log_dist_transit, NA_real_))


cat(sprintf("Income effect significant in: %.1f%% of tracts\n", mean(gwr_sf$income_sig, na.rm = TRUE) * 100))
cat(sprintf("Education effect significant in: %.1f%% of tracts\n", mean(gwr_sf$edu_sig, na.rm = TRUE) * 100))
cat(sprintf("Renter effect significant in: %.1f%% of tracts\n", mean(gwr_sf$renter_sig, na.rm = TRUE) * 100))
cat(sprintf("Transit dist significant in: %.1f%% of tracts\n", mean(gwr_sf$transit_dist_sig, na.rm = TRUE) * 100))

tmap_mode("plot")

## Income Effect
map_income_sig <- tm_shape(gwr_sf) + tm_polygons(fill = "grey90", col = "white", lwd = 0.3) + tm_shape(gwr_sf %>% filter(income_sig)) + 
                  tm_polygons(fill = "log_income", fill.scale = tm_scale_intervals(style = "quantile", n = 7, values = "-RdYlGn", midpoint = 0, value.na = NA),
                              fill.legend = tm_legend(title = "Local Income\nCoefficient"), col = "white", lwd = 0.3) +
                  tm_title("Where Does Income Drive\nHome Values Most?", position = tm_pos_out("center", "top"), size = 0.95, fontface = "bold") +
                  tm_layout(frame = FALSE, legend.outside = TRUE, legend.text.size = 0.6, legend.title.size = 0.7) +
                  tm_credits(paste0(round(mean(gwr_sf$income_sig, na.rm = TRUE) * 100, 1), "% of tracts significant (p < 0.05)\n Grey = not significant"), position = tm_pos_in("left", "bottom"), size = 0.55)

## Education Effect
map_edu_sig <- tm_shape(gwr_sf) + tm_polygons(fill = "grey90", col = "white", lwd = 0.3) + tm_shape(gwr_sf %>% filter(edu_sig)) +
               tm_polygons(fill = "bachelors_pct", fill.scale = tm_scale_intervals(style = "quantile", n = 7, values = "PuRd", value.na = NA), fill.legend = tm_legend(title = "Local Education\nCoefficient"),
                           col = "white", lwd = 0.3) + 
               tm_title("Where Does Education Drive\nHome Values Most?", position = tm_pos_out("center", "top"), size = 0.95, fontface = "bold") +
               tm_layout(frame = FALSE, legend.outside = TRUE, legend.text.size = 0.6, legend.title.size = 0.7) + 
               tm_credits(paste0(round(mean(gwr_sf$edu_sig, na.rm = TRUE) * 100, 1), "% significant | Grey = not significant"), position = tm_pos_in("left", "bottom"), size = 0.55)

## Renter Effect
map_renter_sig <- tm_shape(gwr_sf) + tm_polygons(fill = "grey90", col = "white", lwd = 0.3) + tm_shape(gwr_sf %>% filter(renter_sig)) + tm_polygons(fill = "renter_pct", 
                  fill.scale = tm_scale_intervals(style = "quantile", n = 7, values = "OrRd", value.na = NA), 
                  fill.legend = tm_legend(title = "Local Renter\nCoefficient"), col = "white", lwd = 0.3) + 
                  tm_title("Where Does Rental Rate\nDepress Home Values?", position = tm_pos_out("center", "top"), size = 0.95, fontface = "bold") +
                  tm_layout(frame = FALSE, legend.outside = TRUE, legend.text.size = 0.6, legend.title.size = 0.7)

## Transit Distance Effect
map_transit_sig <- tm_shape(gwr_sf) + tm_polygons(fill = "grey90", col = "white", lwd = 0.3) + tm_shape(gwr_sf %>% filter(transit_dist_sig)) + tm_polygons(fill = "log_dist_transit", 
                    fill.scale = tm_scale_intervals(style = "quantile", n = 7, values = "RdYlBu", value.na = NA), fill.legend = tm_legend(title = "Local Transit\nDistance Effect"),
                    col = "white", lwd = 0.3) + 
                   tm_title("Where Does Transit Access\nMatter Most for Prices?", position = tm_pos_out("center", "top"), size = 0.95, fontface = "bold") +
                   tm_layout(frame = FALSE, legend.outside = TRUE, legend.text.size = 0.6, legend.title.size = 0.7)

coef_maps_sig <- tmap_arrange(map_income_sig, map_edu_sig, map_renter_sig, map_transit_sig, ncol = 2)
print(coef_maps_sig)

## Local R2 analysis

r2_summary <- summary(gwr_sf$Local_R2)
print(r2_summary)

cat(sprintf("\nTracts with R² > 0.70: %d (%.1f%%)\n", sum(gwr_sf$Local_R2 > 0.70, na.rm = TRUE), mean(gwr_sf$Local_R2 > 0.70, na.rm = TRUE) * 100))
cat(sprintf("Tracts with R² < 0.40: %d (%.1f%%)\n", sum(gwr_sf$Local_R2 < 0.40, na.rm = TRUE), mean(gwr_sf$Local_R2 < 0.40, na.rm = TRUE) * 100))

local_r2_map <- tm_shape(gwr_sf) + tm_polygons(fill = "Local_R2", 
                                               fill.scale = tm_scale_intervals(style = "quantile", n = 7, values = "RdYlGn", value.na = NA), 
                                               fill.legend = tm_legend(title = "Local R-Square", format = list(digits = 2)), col = "white", lwd = 0.4) + 
                tm_title("GWR Local R² - Model Fit by Census Tract", position = tm_pos_out("center", "top"), size = 1.0, fontface = "bold") +
                tm_layout(frame = FALSE, legend.outside = TRUE, legend.title.size = 0.85, legend.text.size = 0.7) +
                tm_compass(type = "arrow", position = tm_pos_in("left", "bottom"), size = 1.5) +
                tm_scalebar(position = tm_pos_in("left", "bottom"), text.size = 0.6) +
                tm_credits("Green = model fits well (high R-square)\nRed = unexplained variation (low R-square)\nLow R-square areas suggest missing variables",
                  position = tm_pos_in("right", "bottom"), size = 0.52)

print(local_r2_map)

## Combine local R-square with home values
housing <- housing %>% mutate(gwr_local_r2 = gwr_sf$Local_R2)

cat("\n Tracts where model performs worst (R² < 0.40)\n")

low_r2_tracts <- housing %>% st_drop_geometry() %>% filter(gwr_local_r2 < 0.80) %>% select(tract_name, home_value, income, bachelors_pct, n_transit, gwr_local_r2) %>%
                  arrange(gwr_local_r2)

print(low_r2_tracts)

saveRDS(housing, "data/processed/housing_interpreted.rds")


