# Title: Local Indicator of Spatial Association (LISA) analysis
# File: R/04_spatial_autocorrelation/03_lisa_analysis.R
# Project: Real Estate Price Prediction with Spatial Autocorrelation
# Description: Runs Local Moran's I (LISA), classifies tracts into cluster types, and maps the hot and cold spots.

library(tidyverse)
library(sf)
library(spdep)
library(tmap)

housing <- readRDS("data/processed/housing_final.rds")
weights_queen <- readRDS("data/processed/weights_queen.rds")
moran_df <- readRDS("data/processed/moran_df.rds")

## Local Moran's I
set.seed(42)

lisa_results <- localmoran_perm(housing$log_home_value, listw = weights_queen, nsim = 999, zero.policy = TRUE, alternative = "two.sided")
cat(colnames(lisa_results))

## Classify into LISA clusters
# Label a tract as cluster member if
#     - It's local moran's I is statistically significant (p < 0.05)
#     - it falls in one of the four meaningful quadrants

housing <- housing %>% mutate(lisa_i = lisa_results[, "Ii"], lisa_pval = lisa_results[, "Pr(z != E(Ii))"],
                              sig_05 = lisa_pval < 0.05, sig_01 = lisa_pval < 0.01, 
                              z_value = moran_df$z_value,
                              lag_z = moran_df$lag_z_value,
                              lisa_cluster = case_when(
                                sig_05 & z_value > 0 & lag_z > 0 ~ "High-High",
                                sig_05 & z_value < 0 & lag_z < 0 ~ "Low-Low",
                                sig_05 & z_value > 0 & lag_z < 0 ~ "High-Low",
                                sig_05 & z_value < 0 & lag_z > 0 ~ "Low-High",
                                TRUE ~ "Not Significant"
                              ),
                              lisa_cluster = factor(lisa_cluster, levels = c("High-High", "Low-Low", "High-Low", "Low-High", "Not Significant")))



housing %>% 
  st_drop_geometry() %>% 
  as_tibble() %>% 
  count(lisa_cluster) %>% 
  mutate(
    pct = round(n / sum(n) * 100, 1),
    interpretation = case_when(
      lisa_cluster == "High-High" ~ "Expensive tracts surrounded by expensive tracts",
      lisa_cluster == "Low-Low" ~ "Affordable tracts surrounded by affordable tracts",
      lisa_cluster == "High-Low" ~ "Expensive tract surrounded by affordable tracts",
      lisa_cluster == "Low-High" ~ "Affordable tract surrounded by expensive tracts",
      lisa_cluster == "Not Significant" ~ "No significant local clustering"
    )
  ) %>% 
  print(width = Inf)

lisa_colors <- c("High-High" = "#D7191C", "Low-Low" = "#2C7BB6", "High-Low" = "#FC8D59", "Low-High" = "#ABD9E9", "Not Significant" = "#F0F0F0")
tmap_mode("plot")


# LISA Significance Map
lisa_sig_map <- tm_shape(housing) + 
  tm_polygons(fill = "lisa_pval", 
              fill.scale = tm_scale_intervals(breaks = c(0, 0.01, 0.05, 0.1, 1),
                                              labels = c("p < 0.01", "p < 0.05", "p < 0.10", "Not significant"),
                                              values = c("#D7191C", "#FC8D59", "#FEE090", "#F0F0F0")),
              fill.legend = tm_legend(title = "Local Moran's I\nP-value"),
              col = "grey30", lwd = 0.3) +
  tm_title("LISA Significance Map", size = 1.0, fontface = "bold", position = tm_pos_out("center", "top")) + 
  tm_layout(frame = FALSE, legend.outside = TRUE)

print(lisa_sig_map)

# Home Value Map
home_val_map <- tm_shape(housing) + 
  tm_polygons(fill = "home_value", 
              fill.scale = tm_scale_intervals(style = "quantile", n = 7, values = "YlOrRd"), 
              fill.legend = tm_legend(title = "Home Value ($)", 
                                      format = list(fun = function(x) scales::dollar(x, scale = 1/1000, suffix = "K"))),
              col = "grey30", lwd = 0.3) + 
  tm_title("Median Home Values", size = 1.0, fontface = "bold", position = tm_pos_out("center", "top")) + 
  tm_layout(frame = FALSE, legend.outside = TRUE)

# LISA Cluster Map
lisa_colors <- c("High-High" = "#D7191C", "Low-Low" = "#2C7BB6", "High-Low" = "#FC8D59", "Low-High" = "#ABD9E9", "Not Significant" = "#F0F0F0")

lisa_map_clean <- tm_shape(housing) + 
  tm_polygons(fill = "lisa_cluster", 
              fill.scale = tm_scale_categorical(values = lisa_colors), 
              fill.legend = tm_legend(title = "Cluster Type"),
              col = "grey30", lwd = 0.3) + 
  tm_title("LISA Cluster Map - Home Values\nTravis County, TX", size = 1.0, fontface = "bold", position = tm_pos_out("center", "top")) +
  tm_layout(legend.outside = TRUE, frame = FALSE)

adj <- tmap_arrange(home_val_map, lisa_map_clean, ncol = 2)
print(adj)

saveRDS(housing, "data/processed/housing_with_lisa.rds")


