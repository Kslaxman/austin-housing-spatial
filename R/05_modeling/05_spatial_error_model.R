# Title: Spatial Error Model
# File: R/05_modeling/05_spatial_error_model.R
# Project: Real Estate Price Prediction with Spatial Autocorrelation
# Description: Fits the Spatial Error (SEM) model; extracts lambda, pseudo-R2, AIC, 
# and residual Moran's I.

library(tidyverse)
library(sf)
library(spdep)
library(spatialreg)

housing <- readRDS("data/processed/housing_spat.rds")
ols_model <- readRDS("outputs/models/ols_model.rds")
spat_model <- readRDS("outputs/models/spat_model.rds")
weights_queen <- readRDS("data/processed/weights_queen.rds")

hdf <- housing %>% st_drop_geometry()
ols_formula <- formula(ols_model)

sem_model <- errorsarlm(formula = ols_formula, data = hdf, listw = weights_queen, method = "eigen", zero.policy = TRUE)
print(summary(sem_model))

lambda <- sem_model$lambda
lambda_se <- sem_model$lambda.se
lambda_z <- lambda / lambda_se
lambda_pval <- 2 * pnorm(-abs(lambda_z))

cat(sprintf("λ:    %0.4f\n", lambda))
cat(sprintf("Std Error:     %0.4f\n", lambda_se))
cat(sprintf("Z-score:       %0.4f\n", lambda_z))
cat(sprintf("P-value:       %0.6f\n", lambda_pval))

## SEM residuals
sem_residuals <- residuals(sem_model)

moran_sem <- moran.test(sem_residuals, listw = weights_queen, zero.policy = TRUE)

sem_aic <- AIC(sem_model)
spat_aic <- AIC(spat_model)
ols_aic <- AIC(ols_model)

cat(sprintf("OLS AIC: %0.2f\n", ols_aic))
cat(sprintf("SLM AIC: %0.2f\n", spat_aic))
cat(sprintf("SEM AIC: %0.2f\n", sem_aic))

best_one <- if(sem_aic < spat_aic) "SEM" else "SLM"
cat(sprintf("\n Best model: %s\n", best_one))

saveRDS(sem_model, "outputs/models/sem_model.rds")
saveRDS(housing, "data/processed/housing_sem.rds")
