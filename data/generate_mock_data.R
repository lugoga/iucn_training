# IUCN Bahari Yetu: Mock Data Generator
# This script generates mock datasets for the 7 research themes.
# These datasets are used throughout the hands-on daily exercises.

library(tidyverse)
library(here)

# Create data directory if it doesn't exist
data_dir <- here("module", "data")
if (!dir.exists(data_dir)) {
  dir.create(data_dir, recursive = TRUE)
}

set.seed(123)

# 1. Coastal Forest Theme (Tree DBH, height, species, plots)
coastal_forest <- tibble(
  plot_id = rep(paste0("Plot_", 1:10), each = 10),
  tree_species = sample(c("Avicennia marina", "Rhizophora mucronata", "Sonneratia alba"), 100, replace = TRUE),
  dbh_cm = round(runif(100, 8, 55), 1),
  height_m = round(dbh_cm * 0.58 + rnorm(100, 0, 1.8), 1)
)
write_csv(coastal_forest, file.path(data_dir, "theme1_coastal_forest.csv"))

# 2. GHG - IPPU (Cement production activity and factors)
ghg_ippu <- tibble(
  facility_id = rep(paste0("FAC_", 1:4), each = 5),
  year = rep(2020:2024, times = 4),
  cement_produced_tonnes = round(runif(20, 8000, 25000)),
  clinker_factor = round(runif(20, 0.75, 0.95), 2)
)
write_csv(ghg_ippu, file.path(data_dir, "theme2_ghg_ippu.csv"))

# 3. GHG - Energy (Household energy surveys)
ghg_energy <- tibble(
  household_id = paste0("HH_", 101:150),
  district = sample(c("Urban", "West", "North", "South"), 50, replace = TRUE),
  occupants = sample(2:10, 50, replace = TRUE),
  charcoal_used_kg_week = round(runif(50, 5, 25), 1),
  firewood_used_kg_week = round(runif(50, 10, 45), 1)
)
write_csv(ghg_energy, file.path(data_dir, "theme3_ghg_energy.csv"))

# 4. Marine Ecology (Dolphin acoustics & boat traffic index)
marine_ecology <- tibble(
  survey_date = rep(seq(as.Date("2026-01-01"), as.Date("2026-01-10"), by = "days"), each = 3),
  site = rep(c("Kigamboni", "Coco Beach", "Msasani"), times = 10),
  boat_density_index = round(runif(30, 0.1, 0.95), 2),
  dolphin_detections = round(50 - (boat_density_index * 40) + rnorm(30, 0, 4))
)
write_csv(marine_ecology, file.path(data_dir, "theme4_marine_ecology.csv"))

# 5. GHG - AFOLU (Agriculture yield & soil carbon)
ghg_afolu <- tibble(
  farm_id = paste0("FARM_", 1:40),
  district = sample(c("Mvomero", "Kilosa", "Morogoro Rural"), 40, replace = TRUE),
  crop_type = sample(c("Maize", "Rice"), 40, replace = TRUE),
  yield_tonnes_ha = round(runif(40, 1.2, 5.8), 2),
  fertilizer_nitrogen_kg_ha = round(runif(40, 0, 120), 1),
  soil_organic_carbon_pct = round(runif(40, 1.5, 4.5), 2)
)
write_csv(ghg_afolu, file.path(data_dir, "theme5_ghg_afolu.csv"))

# 6. Plastic Waste Management (Macroplastics survey coordinates)
plastic_waste <- tibble(
  station_id = paste0("ST_", 1:30),
  location = sample(c("Dar es Salaam Shore", "Zanzibar Coast", "Tanga Port"), 30, replace = TRUE),
  lon = round(runif(30, 39.15, 39.35), 4),
  lat = round(runif(30, -6.85, -6.65), 4),
  macroplastics_count = round(runif(30, 20, 500))
)
write_csv(plastic_waste, file.path(data_dir, "theme6_plastic_waste.csv"))

# 7. GHG - Waste (Anaerobic digestion digestion rates)
ghg_waste <- tibble(
  reactor_id = rep(paste0("Reactor_", LETTERS[1:4]), each = 6),
  feedstock = rep(c("Food Waste", "Manure", "Market Waste", "Co-digested"), each = 6),
  day = rep(1:6, times = 4),
  daily_methane_yield_ml_g = round(rep(c(10, 18, 14, 25), each = 6) * log(1:6 + 1) + rnorm(24, 0, 1), 2)
)
write_csv(ghg_waste, file.path(data_dir, "theme7_ghg_waste.csv"))

print("Successfully generated all theme datasets in module/data/.")
