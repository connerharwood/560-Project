library(dplyr)
library(tidyverse)
library(data.table)

load("~/560-Project/clean-data/data/wateruse_clean.rds")
load("~/560-Project/clean-data/data/gsl_clean.rds")
load("~/560-Project/clean-data/data/precip_clean.rds")
load("~/560-Project/clean-data/data/population_clean.rds")

#------------------------------------------------------------------------------#
# merge ----

# merge water use and precipitation data
merge1 = left_join(wateruse_clean, precip_clean, by = c("year", "month", "county"), relationship = "many-to-one")

# merge water use and precipitation data with GSL data
merge2 = left_join(merge1, gsl_clean, by = c("year", "month"), relationship = "many-to-one")

# merge water use, precipitation, and GSL data with population data
merge3 = left_join(merge2, population_clean, by = c("year", "county"), relationship = "many-to-one")

# change merge3 to data table
merge3 = as.data.table(merge3)

# select, reorder, and rename relevant variables
merged1 = merge3 |> 
  select(
    key,
    system_id,
    system_name,
    system_type,
    county,
    latitude,
    longitude,
    year,
    month,
    use_type,
    source_type,
    diversion_type,
    month_use = month_gallons,
    year_use = year_gallons,
    gsl_level_ft = level,
    gsl_volume_m3 = volume_m3,
    precip_in,
    population_thousands
  )

#------------------------------------------------------------------------------#
# additional cleaning ----

# remove duplicates
merged2 = merged1 |> 
  select(-key) |> 
  distinct() |> 
  mutate(
    key = row_number()) |> 
  relocate(
    key, .before = system_id)

master_data = merged2

#------------------------------------------------------------------------------#
# save master dataset ----

save(master_data, file = "master_data.rds")

#------------------------------------------------------------------------------#

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