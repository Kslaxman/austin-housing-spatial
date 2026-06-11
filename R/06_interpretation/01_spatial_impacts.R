# Title: Direct, Indirect, total effects -  Spatial Impacts
# File: R/06_interpretation/01_spatial_impacts.R
# Project: Real Estate Price Prediction with Spatial Autocorrelation
# Description: Decomposes SAR effects into direct, indirect (spillover), and total
# impacts via the spatial multiplier

library(tidyverse)
library(sf)
library(spdep)
library(spatialreg)
library(ggplot2)
library(patchwork)

spat_model <- readRDS("outputs/models/spat_model.rds")
weights_queen <- readRDS("data/processed/weights_queen.rds")
housing <- readRDS("data/processed/housing_interpreted.rds")

## R = Number of simulations for standard error estimation
## Higher R = more stable SE estimates

set.seed(42)

spat_impacts <- impacts(spat_model, listw = weights_queen, R = 1000)
print(summary(spat_impacts, zstats = TRUE, short = TRUE))

impact_summary <- summary(spat_impacts, zstats = TRUE)

var_names <- names(coef(spat_model))
var_names <- var_names[var_names != "(Intercept)"]

impacts_df <- tibble(
                  variable = rownames(impact_summary$zmat), 
                  
                  direct   = as.numeric(impact_summary$res$direct),
                  indirect = as.numeric(impact_summary$res$indirect),
                  total    = as.numeric(impact_summary$res$total),
                  
                  # Z-values
                  direct_z   = as.numeric(impact_summary$zmat[, "Direct"]),
                  indirect_z = as.numeric(impact_summary$zmat[, "Indirect"]),
                  total_z    = as.numeric(impact_summary$zmat[, "Total"]),
                  
                  # P-value (2-sided)
                  direct_p   = 2 * pnorm(-abs(direct_z)),
                  indirect_p = 2 * pnorm(-abs(indirect_z)),
                  total_p    = 2 * pnorm(-abs(total_z))
                ) %>% 
                  mutate(
                    direct_sig = case_when(
                      direct_p < 0.001 ~ "***", direct_p < 0.01 ~ "**", direct_p < 0.05 ~ "*", direct_p < 0.1 ~ ".", TRUE ~ ""
                    ),
                    indirect_sig = case_when(
                      indirect_p < 0.001 ~ "***", indirect_p < 0.01 ~ "**", indirect_p < 0.05 ~ "*", indirect_p < 0.1 ~ ".", TRUE ~ "" 
                    ),
                    total_sig = case_when(
                      total_p < 0.001 ~ "***", total_p < 0.01 ~ "**", total_p < 0.05 ~ "*", TRUE ~ ""
                    ),
                    
                    # What % of total effect is indirect?
                    spillover = round(abs(indirect) / abs(total) * 100, 1),
                    
                    var_label = case_when(
                      variable == "log_income" ~ "Log Income",
                      variable == "bachelors_pct" ~ "% Bachelor's+",
                      variable == "renter_pct" ~ "% Renter-occupied",
                      variable == "housing_age" ~ "Housing Age (years)",
                      variable == "vacant_pct" ~ "% Vacant Units",
                      variable == "med_age" ~ "Median Age",
                      variable == "log_dist_school" ~ "Log Dist. School",
                      variable == "log_dist_transit" ~ "Log Dist. Transit",
                      variable == "log_dist_park" ~ "Log Dist. Park",
                      variable == "n_transit" ~ "Transit Stops (N)",
                      TRUE ~ variable
                    )
                  ) %>% 
                  arrange(desc(abs(total)))

impacts_df %>% 
  select(var_label, direct, indirect, total, direct_sig, indirect_sig, spillover) %>% 
  mutate(across(c(direct, indirect, total), ~ round(.x, 4))) %>% 
  print(width = Inf)

impacts_long <- impacts_df %>% 
                  mutate(var_label = fct_reorder(var_label, abs(total))) %>% 
                  select(var_label, direct, indirect, total) %>% 
                  pivot_longer(cols = c(direct, indirect, total), 
                               names_to = "effect_type", 
                               values_to = "magnitude") %>%
                  mutate(effect_type = str_to_title(effect_type))

impacts_plot <- ggplot(impacts_long %>% filter(effect_type != "Total"), aes(x = magnitude, y = var_label, fill = effect_type)) + 
                  geom_col(position = "dodge", alpha = 0.85, width = 0.7) +
                  geom_vline(xintercept = 0, color = "black", linewidth = 0.8) + 
                  geom_point(data = impacts_long %>% filter(effect_type == "Total"), 
                             aes(x = magnitude, y = var_label), 
                             color = "#333333", size = 3, shape = 23, fill = "white", stroke = 1.5) +
                  scale_fill_manual(values = c("Direct" = "#2C7BB6", "Indirect" = "#FC8D59"), name = "Effect Type") + 
                  labs(title = "Spatial Lag Model - Direct & Indirect Effects", 
                       subtitle = paste0("Diamond = total effect | Indirect effects flow through spatial neighbors"),
                       x = "Effect on Log Home Value", 
                       y = NULL, 
                       caption = paste0("Positive - higher home value | Negative - lower home value\n", "Data: ACS 2025, Travis County, TX")) +
                  theme_minimal(base_size = 12) + 
                  theme(plot.title = element_text(face = "bold", size = 13), 
                        plot.subtitle = element_text(size = 9, color = "grey40"), 
                        plot.caption = element_text(size = 8, color = "grey50"), 
                        legend.position = "bottom", 
                        panel.grid.major.y = element_blank())

print(impacts_plot)

impacts_df %>% 
      filter(total_p < 0.05) %>% 
      select(var_label, direct, indirect, total, spillover) %>% 
      mutate(across(c(direct, indirect, total), ~ round(.x * 100, 2)), 
             spillover = paste0(spillover, "%")) %>% 
      arrange(desc(as.numeric(str_remove(spillover, "%")))) %>% 
      print(width = Inf)

saveRDS(impacts_df, "outputs/models/spatial_impacts.rds")