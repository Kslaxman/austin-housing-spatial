# Title: Exploratory data analysis
# File: R/03_eda/01_eda_descriptive.R
# Project: Real Estate Price Prediction with Spatial Autocorrelation
# Description: Computes descriptive statistics for outcome and predictors and the zero-amenity tract counts

library(tidyverse)
library(sf)
library(patchwork)
library(scales)
library(kableExtra)
library(corrr)
library(ggcorrplot)

housing <- readRDS("data/processed/housing_final.rds")

# Dropping geometry for non-spatial operations
hdf <- housing %>% st_drop_geometry()

desc_stats <- hdf %>% select(home_value, income, bachelors_pct, med_age, renter_pct, vacant_pct, housing_age, n_schools, n_parks, n_transit, dist_school_m, dist_transit_m) %>%
              summarise(across(everything(), list(
                mean = ~round(mean(.x, na.rm = TRUE), 1),
                median = ~round(median(.x, na.rm = TRUE), 1),
                sd = ~round(sd(.x, na.rm = TRUE), 1),
                min = ~round(min(.x, na.rm = TRUE), 1),
                max = ~round(max(.x, na.rm = TRUE), 1)
              ))) %>% pivot_longer(everything(), names_to = c("variable", "stat"), names_sep = "_(?=[^_]+$)") %>% pivot_wider(names_from = stat, values_from = value)

print(desc_stats)

## Home value distribution
p1 <- ggplot(hdf, aes(x = home_value)) + geom_histogram(bins = 40, fill = "#2C7BB6", color = "white", alpha = 0.85) + 
        geom_vline(xintercept = median(hdf$home_value, na.rm = TRUE), color = "#D7191C", linewidth = 1, linetype = "dashed") +
        annotate("text", x = median(hdf$home_value, na.rm = TRUE) * 1.05, y = Inf, label = paste0("Median: ", dollar(median(hdf$home_value, na.rm = TRUE))), 
                 hjust = 0, vjust = 1.5, color = "#D7191C", size = 3.5) + scale_x_continuous(labels = dollar_format(scale = 1/1000, suffix = "K")) + 
        labs(title = "Raw Home Values - Right Skewed", subtitle = "A few very expensive tracts pull the distribution right", x = "Median Home Value", y = "Number of Tracts") +
        theme_minimal(base_size = 12) + theme(plot.title = element_text(face = "bold"), panel.grid = element_blank())

p2 <- ggplot(hdf, aes(x = log_home_value)) + geom_histogram(bins = 40, fill = "#1A9641", color = "white", alpha = 0.85) + geom_vline(xintercept = median(hdf$log_home_value, na.rm = TRUE), color = "#D7191C", linewidth = 1, linetype = "dashed") + 
      annotate("text", x = median(hdf$log_home_value, na.rm = TRUE) + 0.05, y = Inf, label = paste0("Median: ", round(median(hdf$log_home_value, na.rm = TRUE), 2)), hjust = 0, vjust = 1.5, color = "#D7191C", size = 3.5) + 
      labs(title = "Log-Transformed Home Values - Approximately Normal", subtitle = "Symmetrizes the distribution for modeling", x = "Log(Median Home Value)", y = "Number of Tracts") +
      theme_minimal(base_size = 12) + theme(plot.title = element_text(face = "bold"), panel.grid = element_blank())

dist_plot <- p1 + p2 + plot_annotation(title = "Home values", caption = "Data: US Census ACS Estimates 2025, Travis County TX")
print(dist_plot)

ggsave("outputs/maps/01_home_value_distribution.png", plot = dist_plot, width  = 12, height = 5, dpi = 300)

## Distribution of key predictors
vars_to_plot <- c("income", "bachelors_pct", "renter_pct", "med_age", "vacant_pct", "housing_age")
var_labels <- c("income" = "Median Household Income ($)", "bachelors_pct" = "% With Bachelor's Degree", "renter_pct" = "% Renter-Occupied Units", "med_age" = "Median Age", "vacant_pct" = "% Vacant Units", "housing_age" = "Median Housing Age (Years)")
predictor_plots <- vars_to_plot %>% map(function(var) {
  ggplot(hdf, aes(x = .data[[var]])) + geom_histogram(bins = 30, fill = "#4575B4", color = "white", alpha = 0.8) + labs(title = var_labels[var], x = NULL, y = "Tracts") +
    theme_minimal(base_size = 10) + theme(plot.title = element_text(face = "bold", size = 10, panel.grid = element_blank()))
})

predictor_grid <- wrap_plots(predictor_plots, ncol = 3)
print(predictor_grid)

ggsave("outputs/maps/02_predictor_distributions.png", plot = predictor_grid, width = 12, height = 7, dpi = 300)

hdf %>% summarise(total_schools = sum(n_schools), total_parks = sum(n_parks), total_transit = sum(n_transit), total_hospitals = sum(n_hospitals), 
                  avg_schools_tract = round(mean(n_schools), 2), avg_parks_tract = round(mean(n_parks), 2), avg_transit_tract = round(mean(n_transit), 2),
                  no_school_pct = round(mean(n_schools == 0) * 100, 1), no_park_pct = round(mean(n_parks == 0) * 100, 1), no_transit_pct = round(mean(n_transit == 0) * 100, 1)) %>%
        glimpse()


## Correlation
corr_vars <- hdf %>% select(log_home_value, log_income, bachelors_pct, renter_pct, vacant_pct, med_age, housing_age, n_schools, n_transit, n_parks, log_dist_school,
                            log_dist_transit, log_dist_park, log_dist_hospital) %>% drop_na()

cor_matrix <- cor(corr_vars, method = "pearson")
rownames(cor_matrix) <- colnames(cor_matrix) <- c("Log Home Value", "Log Income", "% Bachelors", "% Renter", "% Vacant", "Median Age", "Housing Age", "N Schools", "N Transit",
                                                  "N Parks", "Dist School (log)", "Dist Transit (log)", "Dist park (log)", "Dist Hospital (log)")
cor_plot <- ggcorrplot(cor_matrix, method = "square", type = "lower", lab = TRUE, lab_size = 2.5, colors = c("#D7191C", "white", "#2C7BB6"), title = "Correlation Matrix: Housing Variables (Austin, Travis County, TX)",
                                                                                                             ggtheme = theme_minimal(base_size = 10)) +
                       theme(plot.title = element_text(face = "bold", size = 12), axis.text.x = element_text(angle = 45, hjust = 1, size = 8), axis.text.y = element_text(size = 8), panel.grid = element_blank())
                       
print(cor_plot)

ggsave("outputs/maps/03_correlation_matrix.png", plot = cor_plot, width  = 11, height = 9, dpi = 300)

## Correlation with home value
cor_with_outcome <- cor_matrix["Log Home Value", ] %>% as_tibble(rownames = "variable") %>% rename(correlation = value) %>% filter(variable != "Log Home Value") %>%
                      arrange(desc(abs(correlation)))

print(cor_with_outcome, n = 20)
