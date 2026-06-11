# Title: Geographically Weighted Model
# File: R/05_modeling/06_gwr_model.R
# Project: Real Estate Price Prediction with Spatial Autocorrelation
# Description: Fits GWR: selects adaptive bisquare bandwidth by cross-validation,
# extracts local coefficients, local R2, and AICc.

library(tidyverse)
library(sf)
library(GWmodel)
library(tmap)
library(patchwork)

housing <- readRDS("data/processed/housing_sem.rds")
ols_model <- readRDS("outputs/models/ols_model.rds")

housing_utm <- housing %>% st_transform(32614) # UTM Zone 14N - distances in meters
housing_sp <- as(housing_utm, "Spatial")

gwr_vars <- c("log_income", "bachelors_pct", "renter_pct", "housing_age", "vacant_pct", "med_age", "log_dist_school",  "log_dist_transit", "log_dist_park", "n_transit")
missing_vars <- gwr_vars[!gwr_vars %in% names(housing_utm)]
if(length(missing_vars) > 0) {
  cat("Missing variables: ", missing_vars, "\n")
}

gwr_formula <- formula(ols_model)

set.seed(42)

gwr_bandwidth <- bw.gwr(formula = gwr_formula, data = housing_sp, approach = "CV", kernel = "bisquare", adaptive = TRUE)
cat(sprintf("Optimal bandwidth: %d nearest neighbors\n\n", round(gwr_bandwidth)))

gwr_model <- gwr.basic(formula  = gwr_formula, data = housing_sp, bw = gwr_bandwidth, kernel = "bisquare", adaptive = TRUE, F123.test = FALSE)
print(gwr_model)

gwr_sf <- gwr_model$SDF %>% st_as_sf() %>% st_transform(4326)

housing <- housing %>% mutate(gwr_local_r2 = gwr_sf$Local_R2,
                              gwr_residual = gwr_sf$residual)

cat("Local coefficient ranges: \n")
coef_cols <- c("log_income", "bachelors_pct", "renter_pct", "housing_age")

for(col in coef_cols) {
  vals <- gwr_sf[[col]]
  cat(sprintf(" %-20s min: %+0.4f | median: %+0.4f | max: %+0.4f\n", col, min(vals, na.rm = TRUE), median(vals, na.rm = TRUE), max(vals, na.rm = TRUE)))
}

## Coefficient Maps

tmap_mode("plot")

gwr_map <- function(data, variable, title, palette = "RdYlGn") {
  tm_shape(data) + 
    tm_polygons(fill = variable,
                fill.scale = tm_scale_intervals(style = "quantile", n = 5, values = palette, value.na = NA), 
                fill.legend = tm_legend(title = "Coefficient"),
                col = "grey30", lwd = 0.3) +
    tm_title(title, position = tm_pos_out("center", "top"), size = 0.9, fontface = "bold") +
    tm_layout(frame = FALSE, legend.outside = TRUE)
}

map_income    <- gwr_map(gwr_sf, "log_income", "Local: Income Effect", "RdYlGn")
map_education <- gwr_map(gwr_sf, "bachelors_pct", "Local: Education Effect", "RdYlGn")
map_renter    <- gwr_map(gwr_sf, "renter_pct", "Local: Renter Effect", "RdYlBu")
map_age       <- gwr_map(gwr_sf, "housing_age", "Local: Housing Age Effect", "RdYlBu")

map_r2 <- tm_shape(gwr_sf) + 
  tm_polygons(fill = "Local_R2", 
              fill.scale = tm_scale_intervals(style = "quantile", n = 7, values = "Blues", value.na = NA), 
              fill.legend = tm_legend(title = "Local R²"),
              col = "grey30", lwd = 0.3) +
  tm_title("GWR Local R² - Model Fit by Tract", 
           position = tm_pos_out("center", "top"), 
           size = 1.0, 
           fontface = "bold") +
  tm_layout(frame = FALSE, legend.outside = TRUE)

print(map_r2)

coef_maps <- tmap_arrange(map_income, map_education, map_renter, map_age, ncol = 2)
print(coef_maps)

saveRDS(gwr_model, "outputs/models/gwr_model.rds")
saveRDS(gwr_sf, "data/processed/gwr_results.rds")
saveRDS(housing, "data/processed/housing_gwr.rds")