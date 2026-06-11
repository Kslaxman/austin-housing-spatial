# Title: Exploratory data analysis plots
# File: R/03_eda/03_eda_plots.R
# Project: Real Estate Price Prediction with Spatial Autocorrelation
# Description: Produces non-spatial graphics raw vs log-distribution, predictor histograms, correlation
# matrix and scatterplots

library(tidyverse)
library(sf)
library(patchwork)
library(scales)

housing <- readRDS("data/processed/housing_final.rds")
hdf <- housing %>% st_drop_geometry()

make_scatter <- function(data, x_var, x_label, point_color = "#2C7BB6") {
  r_val <- cor(data[[x_var]], data$log_home_value, use = "complete.obs")
  ggplot(data, aes(x = .data[[x_var]], y = log_home_value)) + 
    geom_point(color = point_color, alpha = 0.5, size = 1.8) +
    geom_smooth(method = "lm", se = TRUE, color = "#D7191C", fill = "#D7191C", alpha = 0.15, linewidth = 1) +
    annotate("text", x = -Inf, y = Inf, label = paste0("r = ", round(r_val, 2)), hjust = -0.2, vjust = 1.5, size = 4, fontface = "bold", color = "#333333") +
    labs(x = x_label, y = "Log(Home Value)", title = x_label) + 
    theme_minimal(base_size = 10) + 
    theme(plot.title = element_text(face = "bold", size = 10), axis.title = element_text(size = 9))
}

s1 <- make_scatter(hdf, "log_income", "Log Median Income", "#1A9641")
s2 <- make_scatter(hdf, "bachelors_pct", "% Bachelor's Degree+", "#2C7BB6")
s3 <- make_scatter(hdf, "renter_pct", "% Renter Occupied", "#D7191C")
s4 <- make_scatter(hdf, "med_age", "Median Age", "#7B2D8B")
s5 <- make_scatter(hdf, "housing_age", "Median Housing Age (Years)", "#FC8D59")
s6 <- make_scatter(hdf, "vacant_pct", "% Vacant Units", "#636363")
s7 <- make_scatter(hdf, "log_dist_school", "Log Dist to School (m)", "#2166AC")
s8 <- make_scatter(hdf, "log_dist_transit", "Log Dist to Transit (m)", "#4DAC26")
s9 <- make_scatter(hdf, "n_transit", "Transit Stops in Tract", "#8073AC")

scatter_grid <- (s1 | s2 | s3) / (s4 | s5 | s6) / (s7 | s8 | s9) + plot_annotation(title = "Predictor Relationships with Log Home Value",
                                                                                   subtitle = "Travis County, TX - each point is a census tract | red line = OLS fit | r = Pearson correlation",
                                                                                   caption = "Data: US Census ACS 2025 + NCES + CapMetro GTFS + City of Austin",
                                                                                   theme = theme(
                                                                                     plot.title = element_text(face = "bold", size = 14),
                                                                                     plot.subtitle = element_text(size = 9, color = "grey40"),
                                                                                     plot.caption = element_text(size = 8, color = "grey50")
                                                                                   ))

print(scatter_grid)

top_predictors <- tibble(variable = c("log_income", "bachelors_pct", "renter_pct", "housing_age", "log_dist_transit", "vacant_pct", "med_age")) %>%
                  mutate(r = map_dbl(variable, ~cor(hdf[[.x]], hdf$log_home_value, use = "complete.obs"))) %>% arrange(desc(abs(r)))
cat("\nTop predictors by correlation with log home value")
print(top_predictors)
