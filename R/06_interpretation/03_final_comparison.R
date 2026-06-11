# Title: Final Model Comparison
# File: R/06_interpretation/03_final_comparison.R
# Project: Real Estate Price Prediction with Spatial Autocorrelation
# Description: Selects final model by out-of-sample accuracy (RMSE/MAE/MAPE)

library(tidyverse)
library(sf)
library(spdep)
library(gt)
library(tmap)

# ---- Load data and models ----
housing       <- readRDS("data/processed/housing_gwr.rds")
ols_model     <- readRDS("outputs/models/ols_model.rds")
spat_model    <- readRDS("outputs/models/spat_model.rds")
sem_model     <- readRDS("outputs/models/sem_model.rds")
gwr_model     <- readRDS("outputs/models/gwr_model.rds")
weights_queen <- readRDS("data/processed/weights_queen.rds")

hdf <- housing %>% st_drop_geometry()

# ---- Helper: Moran's I on a residual vector ----
get_moran <- function(model_resids, weights) {
  test <- moran.test(model_resids, weights, zero.policy = TRUE)
  list(I = round(test$estimate[1], 4), p = round(test$p.value, 4))
}

moran_ols  <- get_moran(residuals(ols_model),  weights_queen)
moran_spat <- get_moran(residuals(spat_model), weights_queen)
moran_sem  <- get_moran(residuals(sem_model),  weights_queen)
moran_gwr  <- get_moran(gwr_model$SDF$residual, weights_queen)

# ---- GWR AICc comes from the model's diagnostics, not AIC() ----
# (gwr.basic objects aren't a standard model class, so AIC() won't work on them)
gwr_aicc <- round(gwr_model$GW.diagnostic$AICc, 1)

# ---- Build the comparison table ----
comp_table <- tibble(
  `#` = 1:4,
  Model = c("OLS", "Spatial Lag (SAR)", "Spatial Error (SEM)", "GWR"),
  Key_Parameter = c("-",
                    paste0("\u03c1 = ", round(spat_model$rho, 3)),
                    paste0("\u03bb = ", round(sem_model$lambda, 3)),
                    paste0("BW = ", round(gwr_model$GW.argument$bw), " tracts")),
  AIC = c(round(AIC(ols_model),  1),
          round(AIC(spat_model), 1),
          round(AIC(sem_model),  1),
          gwr_aicc),                                  # FIX 1: GWR AICc instead of NA
  Moran_I = c(moran_ols$I, moran_spat$I, moran_sem$I, moran_gwr$I),
  Moran_P = c(moran_ols$p, moran_spat$p, moran_sem$p, moran_gwr$p),  # FIX 2: $p, not $I
  # FIX 3: "Yes" means residual autocorrelation is STILL present (p < 0.05)
  Spatial_auto = c(ifelse(moran_ols$p  < 0.05, "Yes", "No"),
                   ifelse(moran_spat$p < 0.05, "Yes", "No"),
                   ifelse(moran_sem$p  < 0.05, "Yes", "No"),
                   ifelse(moran_gwr$p  < 0.05, "Yes", "No")),
  best = c("Benchmark only", "Spillover effects",
           "Omitted variable bias", "Spatial heterogeneity")
)

print(comp_table, width = Inf)

# ---- Save ----
saveRDS(comp_table, "outputs/models/comp_table.rds")

comp_table %>%
  gt() %>%
  tab_header(
    title    = md("**Final Model Comparison**"),
    subtitle = md("AIC for OLS/SAR/SEM; *AICc* for GWR. Lower is better.")
  ) %>%
  cols_label(
    Key_Parameter = "Key Parameter",
    Moran_I = "Residual Moran's I",
    Moran_P = "Moran p-value",
    Spatial_auto = "Autocorr. remaining?",
    best = "Use Case"
  ) %>%
  tab_footnote(
    footnote = "GWR uses AICc (effective parameters), so its value isn't strictly on the same scale as the global models' AIC.",
    locations = cells_body(columns = AIC, rows = Model == "GWR")
  ) %>%
  gtsave("outputs/tables/final_model_comparison.html")