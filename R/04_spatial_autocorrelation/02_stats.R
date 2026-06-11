# Title: Global Moran
# File: R/04_spatial_autocorrelation/02_stats.R
# Project: Real Estate Price Prediction with Spatial Autocorrelation
# Description: Computes Global Moran's I for log home value with a 999 permutation 
# Monte carlo test; produces the Moran scatterplot

library(tidyverse)
library(sf)
library(spdep)
library(ggplot2)

housing <- readRDS("data/processed/housing_final.rds")
weights_queen <- readRDS("data/processed/weights_queen.rds")

## Hypothesis - H0: Home values are randomly distributed across tracts
##            - H1: Home values exhibit spatial autocorrelation

## MORAN'S I TEST

moran_test <- moran.test(housing$log_home_value, listw = weights_queen, randomisation = TRUE, zero.policy = TRUE)
print(moran_test)

moran_i <- moran_test$estimate["Moran I statistic"]
moran_exp <- moran_test$estimate["Expectation"]
moran_var <- moran_test$estimate["Variance"]
moran_pval <- moran_test$p.value
moran_z <- moran_test$statistic

cat("Interpretation")
cat(sprintf("Moran's I:       %0.4f\n", moran_i))
cat(sprintf("Expected (H0):   %0.4f\n", moran_exp))
cat(sprintf("Z-score:         %0.4f\n", moran_z))
cat(sprintf("P-value:         %0.6f\n", moran_pval))

if(moran_pval < 0.001) {
  cat("Strong evidence of positive spatial autocorrelation. High-value tracts cluster near high-value tracts")
  cat("Reject H0")
} else if (moran_pval < 0.05) {
  cat("Significant spatial autocorrelation")
} else {
  cat("No spatial autocorrelation")
}

## MONTE CARLO SIMULATION
set.seed(42)

moran_mc <- moran.mc(housing$log_home_value, listw = weights_queen, nsim = 999, zero.policy = TRUE)
print(moran_mc)

cat(sprintf("\nObserved Moran's I:  %0.4f\n", moran_mc$statistic))
cat(sprintf("Simulated mean:      %0.4f\n", mean(moran_mc$res)))
cat(sprintf("Simulated SD:        %0.4f\n", sd(moran_mc$res)))
cat(sprintf("Pseudo p-value:      %0.4f\n", moran_mc$p.value))
cat(sprintf("Rank (out of 1000):  %d\n", sum(moran_mc$res <= moran_mc$statistic)))

z_home <- scale(housing$log_home_value)[, 1]
lag_z_home <- lag.listw(weights_queen, z_home, zero.policy = TRUE)

slope <- lm(lag_z_home ~ z_home)
cat("Moran's I (Slope): ", round(coef(slope)[2], 4))

moran_df <- tibble(geoid = housing$GEOID, tract_name = housing$tract_name, home_value = housing$home_value, z_value = z_home, lag_z_value = lag_z_home) %>%
            mutate(quadrant = case_when(
              z_value > 0 & lag_z_value > 0 ~ "High-High",
              z_value < 0 & lag_z_value < 0 ~ "Low-Low",
              z_value > 0 & lag_z_value < 0 ~ "High-Low",
              z_value < 0 & lag_z_value > 0 ~ "Low-High", 
              TRUE ~ "Undefined"
            ), quadrant = factor(quadrant, levels = c("High-High", "Low-Low", "High-Low", "Low-High")))

moran_df %>% count(quadrant) %>% mutate(pct = round(n / sum(n) * 100, 1)) %>% print()

quad_colors <- c("High-High" = "#D7191C", "Low-Low" = "#2C7BB6", "High-Low" = "#FC8D59", "Low-High" = "#ABD9E9")
moran_scatter <- ggplot(moran_df, aes(x = z_value, y = lag_z_value)) + geom_hline(yintercept = 0, color = "grey60", linewidth = 0.6, linetype = "dashed") +
                  geom_vline(xintercept = 0, color = "grey60", linewidth = 0.6, linetype = "dashed") +
                  geom_smooth(method = "lm", se = TRUE, color = "#333333", fill = "#333333", alpha = 0.1, linewidth = 1.2) +
                    geom_point(aes(color = quadrant), size = 2, alpha = 0.75) + scale_color_manual(values = quad_colors, name = "LISA Quadrant") +
                    annotate("text", x = 1.8, y = 1.8, label = "High-High\n(Hot Spot)", color = "#D7191C", fontface = "bold", size = 3.2) +
                    annotate("text", x = -1.8, y = -1.8, label = "Low-Low\n(Cold Spot)", color = "#2C7BB6", fontface = "bold", size = 3.2) +
                    annotate("text", x = 1.8, y = -1.8, label = "High-Low\n(Outlier)", color = "#FC8D59", fontface = "bold", size = 3.2) +
                    annotate("text", x = -1.8, y = 1.8, label = "Low-High\n(Outlier)", color = "#4393C3", fontface = "bold", size = 3.2) +
                    labs(title = "Moran Scatterplot - Spatial Lag of Log Home Value", subtitle = paste0("Slope = Moran's I = ", round(coef(slope)[2], 4), " | Positive slope confirms spatial clustering"), 
                         x = "Standardized Log Home Value (z-score)", y = "Spatially Lagged Log Home Value\n(Avg of neighbors)",
                         caption = "Data: ACS 2025, Travis County TX | Queen contiguity weights") +
                    theme_minimal(base_size = 12) + 
                    theme(plot.title = element_text(face = "bold", size = 13), plot.subtitle = element_text(size = 9, color = "grey40"), 
                          plot.caption = element_text(size = 8, color = "grey50"), legend.position = "bottom")

print(moran_scatter)

saveRDS(moran_df, "data/processed/moran_df.rds")
