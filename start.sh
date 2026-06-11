#!/usr/bin/env bash
# 

# Run this from the folder that currently contains your 22 .R files.
# Uses `git mv` if the folder is a git repo (preserves history), else plain mv.


set -euo pipefail

mover() { if git rev-parse --is-inside-work-tree >/dev/null 2>&1; then git mv "$@"; else mv "$@"; fi; }
move_if() { [ -f "$1" ] && { mkdir -p "$(dirname "$2")"; mover "$1" "$2"; echo "  moved $1 -> $2"; } || echo "  skip  $1 (not found)"; }

echo "Creating folder structure..."
mkdir -p R/01_data_collection R/02_data_processing R/03_eda \
         R/04_spatial_autocorrelation R/05_modeling R/06_interpretation \
         R/07_prediction data/raw data/processed \
         outputs/models outputs/figures outputs/tables report
touch data/raw/.gitkeep data/processed/.gitkeep \
      outputs/models/.gitkeep outputs/figures/.gitkeep outputs/tables/.gitkeep

echo "Moving scripts..."
move_if setup.R                  R/00_setup.R
move_if pull_census_data.R       R/01_data_collection/01_pull_census_data.R
move_if pull_amenities.R         R/01_data_collection/02_pull_amenities.R
move_if spatial_join.R           R/01_data_collection/03_spatial_join.R
move_if cleaning.R               R/02_data_processing/01_cleaning.R
move_if eda_descriptive.R        R/03_eda/01_eda_descriptive.R
move_if eda_plots.R              R/03_eda/02_eda_plots.R
move_if eda_maps.R               R/03_eda/03_eda_maps.R
move_if spatial_weights.R        R/04_spatial_autocorrelation/01_spatial_weights.R
move_if stats.R                  R/04_spatial_autocorrelation/02_global_moran.R
move_if lisa_analysis.R          R/04_spatial_autocorrelation/03_lisa_analysis.R
move_if ols_model.R              R/05_modeling/01_ols_model.R
move_if ols_diagnostics.R        R/05_modeling/02_ols_diagnostics.R
move_if lagrange_multiplier.R    R/05_modeling/03_lagrange_multiplier.R
move_if spatial_lag_model.R      R/05_modeling/04_spatial_lag_model.R
move_if spatial_error_model.R    R/05_modeling/05_spatial_error_model.R
move_if gwr_model.R              R/05_modeling/06_gwr_model.R
move_if model_comparison.R       R/05_modeling/07_model_comparison.R
move_if spatial_impacts.R        R/06_interpretation/01_spatial_impacts.R
move_if gwr_interpretation.R     R/06_interpretation/02_gwr_interpretation.R
move_if final_comp.R             R/06_interpretation/03_final_comparison.R
move_if prediction_map.R         R/07_prediction/01_prediction_map.R

echo "Done. Review with: git status   (or)   find R -name '*.R'"
