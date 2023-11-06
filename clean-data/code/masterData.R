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
  select(year, use_type, year_gallons) |> 
  group_by(year, use_type) |> 
  summarize(total_use = sum(year_gallons))

# aggregate land use data to yearly total across counties
landUse_yearly = landUse_clean |> 
  group_by(land_use, year) |> 
  summarize(total_acres = sum(acres)) |> 
  mutate(land_use = ifelse(land_use == "AGRICULTURAL", "AG", land_use))

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

#------------------------------------------------------------------------------#
## Additional cleaning with fully merged data ##

masterData = as.data.table(masterData)

# remove duplicates in the master dataset
masterData = masterData |> 
  distinct()

# check structure of master dataset
str(masterData)

# look at use type categories
unique_use_type = unique(masterData$water_use)
unique_use_type

# group similar use type categories
masterData = masterData |> 
  mutate(water_use = ifelse(water_use == "Irrigation", "Agricultural",
                            ifelse(water_use %in% c("Power (Hydro-Elec)", 
                                                    "Power (Fossil-Fuel)", 
                                                    "Power (Geothermal)",
                                                    "Geothermal"),
                                   "Power", water_use))) |> 
  # remove sewage since only has a few years of observation and small water user
  filter(water_use != "Sewage Treatment" & year >= 1970) 

# check to see if regrouping categories worked
unique_use_type2 = unique(masterData$water_use)
unique_use_type2

# add column that calculates total water usage across all use types per year, and per capita usage per year
masterData2 = masterData |> 
  group_by(year) |> 
  mutate(allUses_total = sum(total_gallons),
         perCapita_usage = allUses_total / population) |> 
  ungroup()

# remove extreme values that are likely invalid data
masterData3 = masterData2 |> 
  filter(perCapita_usage < 139479780)

# look at when mining use type data starts
miningYears = masterData3 |> 
  filter(water_use == "Mining")
min_mining = min(miningYears$year)
min_mining

# look at when power use type data starts
powerYears = masterData3 |> 
  filter(water_use == "Power") |> 
  arrange(year)

# After observing the data, we noticed that mining and power use types did not have data until later years,
# so we checked which year this data started to filter out all years before that
# It turns out the power date starts in 1987, but is missing 1988-1991, so we filter out years before 1992 below

# filter out years before 1992
masterData4 = masterData3 |> 
  filter(year >= 1992)

masterData = masterData4
#------------------------------------------------------------------------------#
# calculate water use per capita
save(masterData, file = "masterData.rds")
