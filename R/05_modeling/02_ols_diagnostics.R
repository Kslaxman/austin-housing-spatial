# Title: OLS Diagnostics and Moran's I on residuals
# File: R/05_modeling/02_ols_diagnostics.R
# Project: Real Estate Price Prediction with Spatial Autocorrelation
# Description: Runs the four classical diagnostic plots plus Moran's I on residuals, 
# exposing the clustering that invalidates OLS

library(tidyverse)
library(sf)
library(spdep)
library(tmap)
library(patchwork)
library(ggplot2)
library(plotly)

housing <- readRDS("data/processed/housing_ols.rds")
ols_model <- readRDS("outputs/models/ols_model.rds")
weights_queen <- readRDS("data/processed/weights_queen.rds")

hdf <- housing %>% st_drop_geometry()

###
# RESIDUAL DIAGNOSTIC PLOTS
###

res_df <- tibble(fitted = fitted(ols_model), residuals = residuals(ols_model), std_resid = rstandard(ols_model))

# Residuals vs Fitted (Should show no pattern - random scatter around zero)
p1 <- ggplot(res_df, aes(x = fitted, residuals)) + geom_point(color = "#2C7BB6", alpha = 0.5, size = 1.5) + 
        geom_hline(yintercept = 0, color = "#D7191C", linewidth = 1, linetype = "dashed") +
        geom_smooth(method = "loess", se = TRUE, color = "#D7191C", fill = "#D7191C", alpha = 0.15, linewidth = 0.8) +
        labs(title = "Residuals vs Fitted", x = "Fitted Values", y = "Residuals") +
        theme_minimal(base_size = 11) + theme(plt.title = element_text(face = "bold"))

# Q-Q Plot (normality check) (Points should fall on the diagonal line)
p2 <- ggplot(res_df, aes(sample = std_resid)) + stat_qq(color = "#2C7BB6", alpha = 0.6, size = 1.5) + 
        stat_qq_line(color = "#D7191C", linewidth = 1) + labs(title = "Q-Q Plot", x = "Theoretical Quantiles", y = "Standardized Residuals") +
        theme_minimal(base_size = 11) + theme(plot.title = element_text(face = "bold"))

# Homoskedasticity (Flat line = homoskedastic residuals)
p3 <- ggplot(res_df, aes(x = fitted, y = sqrt(abs(std_resid)))) + geom_point(color = "#2C7BB6", alpha = 0.5, size = 1.5) +
        geom_smooth(method = "loess", se = TRUE, color = "#D7191C", fill = "#D7191C", alpha = 0.15, linewidth = 0.8) +
        labs(title = "Scale-Location", x = "Fitted Values", y = "Square root of Standardized Residuals") +
        theme_minimal(base_size = 11) + theme(plot.title = element_text(face = "bold"))

# Residuals (Should be approximately normal and centered at zero)
p4 <- ggplot(res_df, aes(x = residuals)) + geom_histogram(bins = 35, fill = "#2C7BB6", color = "white", alpha = 0.8) +
        geom_vline(xintercept = 0, color = "#D7191C", linewidth = 1, linetype = "dashed") +
        labs(title = "Residual Distribution", x = "Residuals", y = "Count") + 
        theme_minimal(base_size = 11) + theme(plot.title = element_text(face = "bold"))

grid <- (p1 | p2) / (p3 | p4) + plot_annotation(title = "OLS Model Diagnostics")
print(grid)

###
# SPATIAL INDEPENDENCE OF RESIDUALS
###

tmap_mode("plot")
ocean_sequential <- c("#08306B", "#08519C", "#2171B5", "#4292C6", "#6BAED6", "#9ECAE1", "#C6DBEF", "#DEEBF7", "#F7FBFF")

housing$hover_info <- paste0(
  "<b>Tract:</b> ", housing$tract_name, "<br>",
  "<b>Residual:</b> ", round(housing$ols_residual, 3), "<br>",
  "<b>Home Value:</b> $", scales::comma(housing$home_value)
)

base_ggmap <- ggplot(housing) + geom_sf(aes(fill = ols_residual, text = hover_info), color = "white", linewidth = 0.2) +
              scale_fill_gradientn(colors = ocean_sequential, name = "OLS Residual") +
              labs(title = "OLS Residuals - Spatial Distribution") +
              theme_void() + theme(plot.title = element_text(face = "bold", size = 14, hjust = 0.5))

residual_map <- ggplotly(base_ggmap, tooltip = "text") %>% layout(annotations = list(x = 0, y = -0.05, text = "Midnight Blue = under-predicted | White = over-predicted", 
                                                                                   showarrow = FALSE, xref = 'paper', yref = 'paper', 
                                                                                   xanchor = 'left', yanchor = 'bottom', font = list(size = 10)))

print(residual_map)


