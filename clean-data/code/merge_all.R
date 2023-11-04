library(dplyr)
library(tidyverse)

load("~/560-Project/clean-data/data/waterUse_clean.rds")
load("~/560-Project/clean-data/data/landUse_clean.rds")
load("~/560-Project/clean-data/data/gsl_levels_clean.rds")

# forgot to convert year in gsl_levels to numeric, so doing that here
gsl_levels = gsl_levels |> 
  mutate(year = as.numeric(year))

# merge water use data with GSL levels data by year and month
waterUse_gslLevels = left_join(waterUse_clean, gsl_levels, by = c("year", "month"), relationship = "many-to-one")

# merge water use and GSL levels data with land use data
waterUse_gslLevels_landUse = left_join(waterUse_gslLevels, landUse_clean, by = c("year", "county"), relationship = "many-to-many")

#------------------------------------------------------------------------------#
## further processing of land use data needed before merging ##

landUse_aggregate = landUse_clean |> 
  select(-description) |> 
  group_by(year, county, land_use) |> 
  summarize(acres = sum(acres))

landUse_clean |> 
  select(description) |> 
  distinct()

landUse_clean |> 
  select(land_use) |> 
  distinct()

#------------------------------------------------------------------------------#

# merge yearly aggregate water-related land use by use type with water use and GSL levels data
waterUse_gslLevels_landUse = left_join(waterUse_gslLevels, landUse_aggregate, 
                                       by = c("year", "county"), 
                                       relationship = "many-to-many")

