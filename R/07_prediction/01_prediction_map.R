# Title: Prediction Map
# File: R/07_prediction/01_prediction_map.R
# Project: Real Estate Price Prediction with Spatial Autocorrelation
# Description: Generates predictions and produces the predicted vs actual plot 
# and interactive leaflet prediction map

library(tidyverse)
library(sf)
library(spdep)
library(spatialreg)
library(tmap)
library(ggplot2)
library(scales)
library(plotly) 
library(mapview)
library(leafsync)

housing <- readRDS("data/processed/housing_interpreted.rds")
ols_model <- readRDS("outputs/models/ols_model.rds")
spat_model <- readRDS("outputs/models/spat_model.rds")
sem_model <- readRDS("outputs/models/sem_model.rds")
weights_queen <- readRDS("data/processed/weights_queen.rds")

hdf <- housing %>% st_drop_geometry()

## OLS Predictions
ols_pred_log <- predict(ols_model, newdata = hdf)

## SLM Predictions (Trend + Signal)
slm_pred_log <- predict(spat_model, newdata = hdf, listw = weights_queen, zero.policy = TRUE, pred.type = "TS") 

## SEM Predictions
sem_pred_log <- predict(sem_model, newdata = hdf, listw = weights_queen, zero.policy = TRUE, pred.type = "TS")

housing <- housing %>% 
              mutate(
                actual_value  = home_value, 
                ols_sigma     = sd(residuals(ols_model)), 
                slm_sigma     = sd(residuals(spat_model)), 
                sem_sigma     = sd(residuals(sem_model)),
                
                ols_predicted = exp(as.numeric(ols_pred_log)) * exp(0.5 * ols_sigma ^ 2), 
                slm_predicted = exp(as.numeric(slm_pred_log)) * exp(0.5 * slm_sigma ^ 2), 
                sem_predicted = exp(as.numeric(sem_pred_log)) * exp(0.5 * sem_sigma ^ 2),
                
                ols_error     = actual_value - ols_predicted,
                slm_error     = actual_value - slm_predicted,
                sem_error     = actual_value - sem_predicted,
                
                ols_error_pct = abs(ols_error / actual_value) * 100,
                slm_error_pct = abs(slm_error / actual_value) * 100,
                sem_error_pct = abs(sem_error / actual_value) * 100
              )

hdf <- housing %>% st_drop_geometry()

accuracy_stats <- tibble(
                    Model = c("OLS", "SLM", "SEM"),
                    RMSE = c(
                      sqrt(mean(housing$ols_error ^ 2, na.rm = TRUE)),
                      sqrt(mean(housing$slm_error ^ 2, na.rm = TRUE)),
                      sqrt(mean(housing$sem_error ^ 2, na.rm = TRUE))
                    ),
                    MAE = c(
                      mean(abs(housing$ols_error), na.rm = TRUE),
                      mean(abs(housing$slm_error), na.rm = TRUE),
                      mean(abs(housing$sem_error), na.rm = TRUE)
                    ),
                    MAPE = c(
                      mean(housing$ols_error_pct, na.rm = TRUE),
                      mean(housing$slm_error_pct, na.rm = TRUE),
                      mean(housing$sem_error_pct, na.rm = TRUE)
                    )
                  ) %>% 
                    mutate(
                      RMSE = dollar(round(RMSE)),
                      MAE  = dollar(round(MAE)),
                      MAPE = paste0(round(MAPE, 1), "%")
                    )

print(accuracy_stats)

best_model <- 'slm'

hdf <- hdf %>% 
  mutate(hover_text = paste0(
    "<b>Tract:</b> ", tract_name, "<br>",
    "<b>Actual Value:</b> ", dollar(actual_value), "<br>",
    "<b>Predicted (SLM):</b> ", dollar(slm_predicted), "<br>",
    "<b>Error:</b> ", dollar(slm_error)
  ))

pred_scatter <- ggplot(hdf, aes(x = slm_predicted, y = actual_value, text = hover_text, color = actual_value)) + 
                geom_point(alpha = 0.7, size = 2) +
                geom_abline(slope = 1, intercept = 0, color = "#4A4A4A", linewidth = 1, linetype = "dashed") +
                geom_smooth(method = "lm", se = TRUE, color = "#E08214", fill = "#E08214", alpha = 0.15, linewidth = 1) +
                scale_color_gradientn(colors = c("#4B0000", "#B30000", "#E65100", "#FF8C00", "#FFD700"), guide = "none") + 
                scale_x_continuous(labels = dollar_format(scale = 1 / 1000, suffix = "K"), limits = c(0, max(hdf$actual_value, na.rm = TRUE) * 1.05)) +
                scale_y_continuous(labels = dollar_format(scale = 1 / 1000, suffix = "K"), limits = c(0, max(hdf$actual_value, na.rm = TRUE) * 1.05)) +
                labs(title = "Spatial Lag Model - Actual vs Predicted Home Values",
                     x = "Predicted Home Value",
                     y = "Actual Home Value") +
                theme_minimal(base_size = 12) +
                theme(plot.title = element_text(face = "bold", size = 13),
                      panel.grid.minor = element_blank())

interactive_scatter <- ggplotly(pred_scatter, tooltip = "text") %>%
                        layout(annotations = list(
                          x = 0, y = -0.08, 
                          text = "Dashed line = perfect prediction | Points above = under-predicted | Below = over-predicted", 
                          showarrow = FALSE, xref = 'paper', yref = 'paper', xanchor = 'left', yanchor = 'top', font = list(size = 10, color = "grey40")
                        ))

print(interactive_scatter)

warm_palette <- hcl.colors(100, palette = "Inferno")

# Actual
map_actual_mv <- mapview(
  housing, 
  zcol = "actual_value",
  col.regions = warm_palette,
  layer.name = "Value",
  color = "white",
  lwd = 0.2,
  alpha.regions = 0.85,
  legend.pos = "bottomright",
  homebutton = FALSE,         
  label = paste0(housing$tract_name, " | Actual: ", dollar(housing$actual_value)) 
)

# Predicted
map_predicted_mv <- mapview(
  housing, 
  zcol = "slm_predicted",
  col.regions = warm_palette,
  layer.name = "Value",
  color = "white",
  lwd = 0.2,
  alpha.regions = 0.85,
  legend = FALSE,             
  homebutton = FALSE,
  label = paste0(housing$tract_name, " | Predicted: ", dollar(housing$slm_predicted)) 
)

sync_maps <- sync(map_actual_mv, map_predicted_mv)
print(sync_maps)

saveRDS(housing, "data/processed/housing_predictions.rds")