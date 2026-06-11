# Title: Lagrange Multiplier
# File: R/05_modeling/03_lagrange_multiplier.R
# Project: Real Estate Price Prediction with Spatial Autocorrelation
# Description: Performs the LM tests (standard and robust, lag and error) and applies
# the Anselin rule to choose the spatial model

library(tidyverse)
library(sf)
library(spdep)
library(spatialreg)

model <- readRDS("outputs/models/ols_model.rds")
weights_queen <- readRDS("data/processed/weights_queen.rds")

# lm.LMtests() runs multiple tests:
#   LMlag - tests for spatial lag dependence (spatial lag model)
#   LMerr - tests for spatial error dependence (spatial error model)
#   RLMlag - robust LM-lag (controlling for error dependence)
#   RLMerr - robust LM-error (controlling for lag dependence)
#   SARMA - tests for both simultaneously

lm_tests <- lm.LMtests(model, listw = weights_queen, test = c("LMlag", "LMerr", "RLMlag", "RLMerr", "SARMA"), zero.policy = TRUE)
print(summary(lm_tests))

lm_df <- tibble(test = c("LM-Lag", "LM-Error", "RLM-Lag", "RLM-Error", "SARMA"), statistic = c(lm_tests$LMlag$statistic, 
                                                                                               lm_tests$LMerr$statistic,
                                                                                               lm_tests$RLMlag$statistic,
                                                                                               lm_tests$RLMerr$statistic,
                                                                                               lm_tests$SARMA$statistic),
                                                                                 p_value = c(lm_tests$LMlag$p.value,
                                                                                             lm_tests$LMerr$p.value,
                                                                                             lm_tests$RLMlag$p.value,
                                                                                             lm_tests$RLMerr$p.value,
                                                                                             lm_tests$SARMA$p.value),
                                                                                 model_implied = c("Spatial Lag Model", 
                                                                                                   "Spatial Error Model",
                                                                                                   "Spatial Lag Model (robust)",
                                                                                                   "Spatial Error Model (robust)",
                                                                                                   "Both")) %>%
        mutate(significant = p_value < 0.05, sig_label = case_when(
          p_value < 0.001 ~ "***", p_value < 0.01 ~ "**", p_value < 0.05 ~ "*", TRUE ~ "ns"
        ))

print(lm_df, width = Inf)


lmlag_sig <- lm_df$significant[lm_df$test == "LM-Lag"]
lmerr_sig <- lm_df$significant[lm_df$test == "Lm-Error"]
rlmlag_sig <- lm_df$significant[lm_df$test == "RLM-Lag"]
rlmerr_sig <- lm_df$significant[lm_df$test == "RLM-Error"]

lmlag_stat <- lm_df$statistic[lm_df$test == "Lm-Lag"]
lmerr_stat  <- lm_df$statistic[lm_df$test == "LM-Error"]
rlmlag_stat <- lm_df$statistic[lm_df$test == "RLM-Lag"]
rlmerr_stat <- lm_df$statistic[lm_df$test == "RLM-Error"]

cat(sprintf("LM-Lag  significant: %s (stat = %.3f)\n", lmlag_sig, lmlag_stat))
cat(sprintf("LM-Error significant: %s (stat = %.3f)\n", lmerr_sig, lmerr_stat))

if (!isTRUE(lmlag_sig) && !isTRUE(lmerr_sig)) {
  decision <- "OLS"
} else if (isTRUE(lmlag_sig) && !isTRUE(lmerr_sig)) {
  decision <- "SLM"
} else if (!isTRUE(lmlag_sig) && isTRUE(lmerr_sig)) {
  decision <- "SEM"
} else {
  safe_lag_stat <- ifelse(is.na(rlmlag_stat), 0, rlmlag_stat)
  safe_err_stat <- ifelse(is.na(rlmerr_stat), 0, rlmerr_stat)
  
  cat(sprintf("RLM-Lag  significant: %s (stat = %.3f)\n", isTRUE(rlmlag_sig), safe_lag_stat))
  cat(sprintf("RLM-Error significant: %s (stat = %.3f)\n", isTRUE(rlmerr_sig), safe_err_stat))
  
  if (isTRUE(rlmlag_sig) && !isTRUE(rlmerr_sig)) {
    decision <- "SLM"
  } else if (!isTRUE(rlmlag_sig) && isTRUE(rlmerr_sig)) {
    decision <- "SEM"
  } else if (isTRUE(rlmlag_stat > rlmerr_stat)) {
    decision <- "SLM"
  } else {
    decision <- "SEM"
  }
}

cat(sprintf("\nMODEL: %s\n", decision))
saveRDS(list(lm_df = lm_df, decision = decision), "outputs/models/lm_test_results.rds")
