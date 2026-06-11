# Title: Set Up
# File: R/00_setup.R
# Project: Real Estate Price Prediction with Spatial Autocorrelation
# Description: Loads packages, set global options, and defines shared constants (UTM Zone 14N CRS, paths, ggplot)


packages_to_install <- c(
  "sf", "terra", "stars",
  "tidycensus", "tigris",
  "osmdata", 
  "tidyverse", "janitor", "lubridate",
  "spdep", "spatialreg", "GWmodel",
  "gstat", 
  "tmap", "leaflet", "mapview", "ggplot2", "patchwork", "viridis", "RColorBrewer",
  "knitr", "kableExtra", "gt",
  
  "shiny", "shinydashboard", "shinyWidgets"
)

packages <- packages_to_install[
  !(packages_to_install %in% installed.packages()[, "Package"])
]

if(length(packages) > 0) {
  install.packages(packages)
} else {
  cat("Already Installed.")
}
