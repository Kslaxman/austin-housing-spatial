# Title: Model Comparison
# File: R/05_modeling/07_model_comparison.R
# Project: Real Estate Price Prediction with Spatial Autocorrelation
# Description: Assembles the OLS/SAR/SEM/GWR comparison on fit, explained variation,
# key parameter and residual Moran's I.

library(tidyverse)
library(sf)
library(spdep)
library(spatialreg)
library(gt)

housing <- readRDS("data/processed/housing_sem.rds")
ols_model <- readRDS("outputs/models/ols_model.rds")
spat_model <- readRDS("outputs/models/spat_model.rds")
sem_model <- readRDS("outputs/models/sem_model.rds")
weights_queen <- readRDS("data/processed/weights_queen.rds")

hdf <- housing %>% st_drop_geometry()

moran_ols <- moran.test(residuals(ols_model), weights_queen, zero.policy = TRUE)
moran_spat <- moran.test(residuals(spat_model), weights_queen, zero.policy = TRUE)
moran_sem <- moran.test(residuals(sem_model), weights_queen, zero.policy = TRUE)

## r2 = (1 - residual variance / total variance)
pseudo_r2 <- function(model) {
  1 - (var(residuals(model)) / var(hdf$log_home_value))
}

comparison_table <- tibble(
  Model = c("OLS", "Spatial Lag (SAR)", "Spatial Error (SEM)"),
  
  AIC = c(round(AIC(ols_model), 2),
          round(AIC(spat_model), 2),
          round(AIC(sem_model), 2)),
  Log_Likelihood = c(round(as.numeric(logLik(ols_model)), 2),
                     round(as.numeric(logLik(spat_model)), 2),
                     round(as.numeric(logLik(sem_model)), 2)),
  Pseudo_R2 = c(round(summary(ols_model)$r.squared, 4),
                 round(pseudo_r2(spat_model), 4),
                 round(pseudo_r2(sem_model), 4)),
  Spatial_Parameter = c("-",
                        paste0("ρ = ", round(spat_model$rho, 4)),
                        paste0("λ = ", round(sem_model$lambda, 4))),
  Moran_I_Residuals = c(round(moran_ols$estimate[1], 4),
                        round(moran_spat$estimate[1], 4),
                        round(moran_sem$estimate[1], 4)),
  Moran_p_value = c(formatC(moran_ols$p.value, format = 'e', digits = 3),
                    formatC(moran_spat$p.value, format = "e", digits = 3),
                    formatC(moran_sem$p.value, format = "e", digits = 3)),
  Residuals = c(ifelse(moran_ols$p.value > 0.05, "Yes", "No"),
                ifelse(moran_spat$p.value > 0.05, "Yes", "No"),
                ifelse(moran_sem$p.value > 0.05, "Yes", "No"))
)

print(comparison_table, width = Inf)

best_model <- comparison_table$Model[which.min(comparison_table$AIC)]
cat(sprintf("\nBest model by AIC: %s\n", best_model))

gt_table <- comparison_table %>% gt() %>%
              tab_header(title = md("**Spatial Regression Model Comparison**"),
                         subtitle = md("*Travis County, TX - Median Home Values (Log)*")) %>%
              tab_spanner(label = "Model Fit", columns = c(AIC, Log_Likelihood, Pseudo_R2)) %>%
              tab_spanner(label = "Spatial Diagnostics", columns = c(Moran_I_Residuals, Moran_p_value, Residuals)) %>%
              cols_label(Model = "Model", 
                         AIC = "AIC",
                         Log_Likelihood = "Log-Likelihood",
                         Pseudo_R2 = "Pseudo R2",
                         Spatial_Parameter = "Spatial Parameter",
                         Moran_I_Residuals = "Moran's I",
                         Moran_p_value = "P-value",
                         Residuals = "Residuals") %>%
              tab_style(style = cell_fill(color = "#E8F5E9"),
                        locations = cells_body(rows = Model == best_model)) %>%
              tab_footnote(footnote = "Lower AIC = better fit. Residuals OK = Moran's I p > 0.05 (spatially random residuals)",
                           locations = cells_column_labels(AIC)) %>%
              tab_source_note(source_note = "Data: US Census ACS 2025. Spatial weights: Queen contiguity.")

print(gt_table)
gtsave(gt_table, "outputs/tables/model_comparison.html")

saveRDS(comparison_table, "outputs/models/comparison_table.rds")
