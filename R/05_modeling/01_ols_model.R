# Title: OLS Hedonic Regression
# File: R/05_modeling/01_ols_model.R
# Project: Real Estate Price Prediction with Spatial Autocorrelation
# Description: Fits the baseline OLS regression and reports coefficients, R2 and AIC.

library(tidyverse)
library(sf)
library(spdep)
library(spatialreg)
library(broom)
library(kableExtra)

housing <- readRDS("data/processed/housing_final.rds")
weights_queen <- readRDS("data/processed/weights_queen.rds")

hdf <- housing %>% st_drop_geometry()

# Variables:
#   - log_income: Economic capacity of residents
#   - bachelors_pct: Percentage of people with bachelors degree
#   - renter_pct: Tenure Mix
#   - housing_age: Structural depreciation
#   - vacant_pct: Neighborhood distress signal
#   - med_age: Demographic composition
#   - log_dist_school: Proximity to education 
#   - log_dist_transit: Proximity to transit
#   - log_dist_park: Proximity to green space
#   - n_transit: Transit density within tract

ols <- log_home_value ~ log_income + bachelors_pct + renter_pct + housing_age + vacant_pct + med_age + log_dist_school + log_dist_transit + log_dist_park + n_transit
print(ols)

ols_model <- lm(ols, data = hdf)
print(summary(ols_model))

ols_coeffs <- tidy(ols_model, conf.int = TRUE) %>% mutate(significance = case_when(
  p.value < 0.001 ~ "***", p.value < 0.01 ~ "**", p.value < 0.05 ~ "*", p.value < 0.1 ~ ".", TRUE ~ ""), 
  pct_effect = round((exp(estimate) - 1) * 100, 2)) %>% select(term, estimate, std.error, statistic, p.value, conf.low, conf.high, significance, pct_effect)
print(ols_coeffs, n = 20)

# Model fit stats
r2 <- summary(ols_model)$r.squared
adj_r2 <- summary(ols_model)$adj.r.squared
aic <- AIC(ols_model)
bic <- BIC(ols_model)

cat(sprintf("R²:          %0.4f\n", r2))
cat(sprintf("Adjusted R²: %0.4f\n", adj_r2))
cat(sprintf("AIC:         %0.2f\n", aic))
cat(sprintf("BIC:         %0.2f\n", bic))
cat(sprintf("N tracts:    %d\n",    nrow(hdf)))

housing <- housing %>%
  mutate(
    ols_fitted   = fitted(ols_model),
    ols_residual = residuals(ols_model)
  )

saveRDS(ols_model, "outputs/models/ols_model.rds")
saveRDS(housing,   "data/processed/housing_ols.rds")