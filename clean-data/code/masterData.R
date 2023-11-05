library(dplyr)
library(tidyverse)

load("~/560-Project/clean-data/data/waterUse_clean.rds")
load("~/560-Project/clean-data/data/gsl_levels_clean.rds")
load("~/560-Project/clean-data/data/precip_clean.rds")
load("~/560-Project/clean-data/data/landUse_clean.rds")
load("~/560-Project/clean-data/data/countyPopulations_clean.rds")

#------------------------------------------------------------------------------#
## Prepare data for merging by aggregating to yearly level ## 

# forgot to convert year in gsl level dataset to numeric, so doing that here
gsl_levels = gsl_levels |> 
  mutate(year = as.numeric(year))

# forgot to convert year in land use dataset to numeric, so doing that here
landUse_clean = landUse_clean |> 
  mutate(year = as.numeric(year))

# aggregate great salt lake levels to yearly average 
gsl_levels = gsl_levels |> 
  group_by(year) |> 
  summarize(gsl_level = mean(level))

# create a new dataset with total population across counties 
population = countyPopulations |> 
  mutate(population = population_thousands*1000) |> 
  group_by(year) |> 
  summarize(population = sum(population))

# create a new data set with yearly precipitation data for each county
precipitation = precip_clean |> 
  group_by(year, county) |> 
  mutate(year_total = sum(precip_in))

# calculate yearly precipitation data for all counties
precipitation = precipitation |> 
  group_by(year) |> 
  summarize(precipitation = mean(year_total))

# aggregate water use data to yearly total per water use type
waterUse_yearly = waterUse_clean |> 
  group_by(year, use_type) |> 
  summarize(total_use = sum(year_gallons))

# aggregate land use data to yearly total across counties
landUse_yearly = landUse_clean |> 
  group_by(year, land_use) |> 
  summarize(total_acres = sum(acres))

#------------------------------------------------------------------------------#
## Merge to Master Dataset ## 

# merge water use data with GSL levels data by year
water_gsl = left_join(waterUse_yearly, gsl_levels, by = "year", relationship = "many-to-one")

# merge water use and GSL levels data with precipitation data by year
precip_water_gsl = left_join(water_gsl, precipitation, by = "year", relationship = "many-to-one")

# merge water use, GSL levels, precipitation, and population data by year 
pop_precip_water_gsl = left_join(precip_water_gsl, population, by = "year", relationship = "many-to-one")

# merge water use, GSL levels, precipitation, population, and land use data by year 
masterData0 = left_join(pop_precip_water_gsl, landUse_yearly, by = "year", relationship = "many-to-many")

# reorder and rename some variables
masterData = masterData0 |> 
  select(year, gsl_level, population, precipitation, water_use = use_type, total_gallons = total_use, land_use, total_acres)

save(masterData, file = "masterData.rds")
