# Title: Spatial Lag Model
# File: R/05_modeling/04_spatial_lag_model.R
# Project: Real Estate Price Prediction with Spatial Autocorrelation
# Description: Fits the Spatial Lag (SAR) model; extracts rho, pseudo-R2, AIC, and residual Moran's I.

library(tidyverse)
library(sf)
library(spdep)
library(spatialreg)

housing <- readRDS("data/processed/housing_ols.rds")
model <- readRDS("outputs/models/ols_model.rds")
weights_queen <- readRDS("data/processed/weights_queen.rds")

hdf <- housing %>% st_drop_geometry()

ols_formula <- formula(model)

spat_model <- lagsarlm(formula = ols_formula, data = hdf, listw = weights_queen, method = "eigen", zero.policy = TRUE)
print(summary(spat_model))

rho <- spat_model$rho
rho_se <- spat_model$rho.se
rho_z <- rho / rho_se
rho_pval <- 2 * pnorm(-abs(rho_z))
cat(sprintf("ρ: %0.4f\n", rho))
cat(sprintf("Standard Error: %0.4f\n", rho_se))
cat(sprintf("Z-score: %0.4f\n", rho_z))
cat(sprintf("P-value: %0.6f\n", rho_pval))
cat(sprintf(
  "A 1 percent increase in home values in neighboring tracts is associated with a %.2f%% change in this tract's value.\n",
  rho * 100
))

cat("Direct or Indirect effects between tract changes")
spat_impacts <- impacts(spat_model, listw = weights_queen, R = 500)
print(summary(spat_impacts, zstats = TRUE))

spat_aic <- AIC(spat_model)
spat_loglik <- logLik(spat_model)
ols_aic <- AIC(model)

if(ols_aic - spat_aic > 2) {
  cat("SLM substantially better than OLS (Change in AIC > 2)")
} else {
  cat("Minimal improvement from Spatial Lag Model")
}

spat_residuals <- residuals(spat_model)

moran_spat <- moran.test(spat_residuals, listw = weights_queen, zero.policy = TRUE)

cat(sprintf("Moran's I: %0.4f\n", moran_spat$estimate[1]))
cat(sprintf("P-value:   %0.6f\n", moran_spat$p.value))

housing <- housing %>% mutate(spat_residual = spat_residuals)

saveRDS(spat_model, "outputs/models/spat_model.rds")
saveRDS(housing, "data/processed/housing_spat.rds")
