library(dplyr)
library(tidyverse)

load("~/560-Project/clean-data/data/waterUse_clean.rds")
load("~/560-Project/clean-data/data/gsl_levels_clean.rds")
load("~/560-Project/clean-data/data/precip_clean.rds")

# forgot to convert year in gsl_levels to numeric, so doing that here
gsl_levels = gsl_levels |> 
  mutate(year = as.numeric(year))

# merge water use data with GSL levels data by year and month
waterUse_gslLevels = left_join(waterUse_clean, gsl_levels, by = c("year", "month"), relationship = "many-to-one")

# drop irrelevant key variable
waterUse_gslLevels = waterUse_gslLevels |> 
  select(-key)

# merge water use and GSL levels data with precipitation data by year, county, and month
masterData = left_join(waterUse_gslLevels, precip_clean, by = c("year", "county", "month"), relationship = "many-to-one")

# reorder and rename some variables
masterData = masterData |> 
  relocate(month, .after = year) |> 
  rename(gsl_level = level,
         county_precip = precip_in)

save(masterData, file = "masterData.rds")
