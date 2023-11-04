library(dplyr)
library(tidyverse)
library(skimr)

load("~/560-Project/raw-data/data/waterUse_raw.rda")

#------------------------------------------------------------------------------#

## Data cleaning checklist ##

# 1: Convert file formats
# Already done

# 2: Import data and wrangle into a tidy layout
# Completely did not realize it needed to be converted to long format til after going through the cleaning steps,
# so did this at a later step

#------------------------------------------------------------------------------#

# 3: Remove irrelevant, garbage, or empty rows and columns

# select & rename relevant variables from waterUse_raw
waterUse3 = waterUse_raw |> 
  select(
    system_name = "System Name",
    system_id = "System ID",
    source_name = "Source Name",
    source_id = "Source ID",
    latitude = "Lat NAD83",
    longitude = "Lon NAD83",
    source_type = "Source Type",
    diversion_type = "Diversion Type",
    use_type = "Use Type",
    year = "Year",
    Jan:Dec,
    total = "Total"
  )

# select & rename relevant variables from waterUse_info
waterUse_info3 = waterUse_info |> 
  select(
    system_id = "System ID",
    system_type = "System Type",
    county = "County"
  )

# check for empty rows or columns in waterUse3 (there are none)
which(nrow(is.na(waterUse3) | waterUse3 == "") == ncol(waterUse3))
which(ncol(is.na(waterUse3) | waterUse3 == "") == nrow(waterUse3))

# check for empty rows or columns in waterUse_info3 (there are none)
which(nrow(is.na(waterUse_info3) | waterUse_info3 == "") == ncol(waterUse_info3))
which(ncol(is.na(waterUse_info3) | waterUse_info3 == "") == nrow(waterUse_info3))

#------------------------------------------------------------------------------#

# 4: Identify the primary key, or define a surrogate key
# Not necessary at this stage, might do after merging the two dataframes

#------------------------------------------------------------------------------#

# 5: Resolve duplicates

# unique combination of certain variables to identify duplicates in waterUse3
waterUse3 |> 
  count(system_name, system_id, source_name, latitude, longitude, source_type, 
        diversion_type, use_type, year, total) |> 
  filter(n > 1)

# returns same number of rows as waterUse3, so no duplicates
waterUse_distinct = waterUse3 |> 
  distinct()

# unique combination of certain variables to identify duplicates in waterUse_info3
waterUse_info3 |> 
  count(system_id, system_type, county) |> 
  filter(n > 1)

# No duplicates in waterUse3
# Duplicates in waterUse_info3 won't matter since we're just merging with waterUse to get county info

#------------------------------------------------------------------------------#

# 6: Understand the definition, origin, and units of each variable
# Most are self explanatory
# All water usage expressed in gallons

# 7: Rename variables as necessary
# Already did

#------------------------------------------------------------------------------#
# 8: Understand patterns of missing values

# explore waterUse data
skim(waterUse3)

# Ignore missing latitude and longitude values, this seems to be simply lack of info
# We'll still want the water use data from observations missing coordinates

# look at entries with missing year
waterUse_missingYear = waterUse3 |> 
  filter(is.na(year))

# observations missing year have no water use data, so remove these from dataset
waterUse8 = waterUse3 |> 
  filter(!is.na(year))

# explore data again
skim(waterUse8)

# look at entries with missing total_use
waterUse_missingTotalUse = waterUse8 |> 
  filter(is.na(total))

# observations missing total use have no water use data, so remove these from dataset
waterUse8 = waterUse8 |> 
  filter(!is.na(total))

# explore data again
skim(waterUse8)

# NA values for monthly water usage can be assumed to be 0, so change these values to 0
# waterUse8 = waterUse8 |> 
 # mutate(across(jan:dec, ~ifelse(is.na(.), 0, .)))

# convert invalid latitude and longitude to NA
waterUse8 = waterUse8 |> 
  mutate(latitude = ifelse(latitude < 37 | longitude == 0, NA, latitude),
         longitude = ifelse(latitude < 37 | longitude == 0, NA, longitude))

# explore data again
skim(waterUse8)

# explore values of each variable in waterUse_info3
skim(waterUse_info3)

# look at entries with missing counties
waterUse_info3 |> 
  filter(is.na(county))

# the entries with missing counties don't seem to be entries for water rights users, so remove these observations
waterUse_info8 = waterUse_info3 |> 
  filter(!is.na(county))

#------------------------------------------------------------------------------#

# 9: Convert to numeric
# Variables already in correct class

# 10: Convert to date/time
# Not necessary/applicable

# 11: Recode binary variables
# No binary variables

# 12: Convert to factors
# Not necessary at the moment

# 13: Make units and scales consistent
# They already are

#------------------------------------------------------------------------------#

# 14: Perform logical checks on quantitative variables

# look at unique values of certain character variables for waterUse
waterUse8 |> 
  select(source_type) |> 
  unique()

waterUse8 |> 
  select(diversion_type) |> 
  unique()

waterUse8 |> 
  select(use_type) |> 
  unique()

# look at unique values of certain character variables for waterUse_info
waterUse_info8 |> 
  select(system_type) |> 
  unique()

waterUse_info8 |> 
  select(county) |> 
  unique()

# Relevant character variables look as they should, no need for logical checks

#------------------------------------------------------------------------------#

# 15: Clean string variables

# use_type has two categories that could maybe be combined: "Geothermal" and "Power (Geothermal)"
waterUse8 |> 
  select(use_type) |> 
  unique()

# look at observations with use type of "Power (Geothermal)" and "Geothermal"
waterUse15_geothermal = waterUse8 |> 
  filter(use_type == c("Power (Geothermal)", "Geothermal"))

# The 4 observations "Power (Geothermal)" have no water use data, so I'll ignore this for now
# We may also remove observations without 0 water use later

#------------------------------------------------------------------------------#

# create dataset copies for merging
waterUse_merge = waterUse8
waterUse_info_merge = waterUse_info8

# merge waterUse and waterUse_info datasets
waterUse_merged = left_join(waterUse_merge, waterUse_info_merge, 
                             by = "system_id", relationship = "many-to-many")

# drop duplicates, reorder columns, drop source_name
waterUse_almostClean = waterUse_merged |> 
  distinct() |>
  select(-source_name) |> 
  relocate(source_id, .after = system_id) |> 
  relocate(year, .after = source_id) |>
  relocate(county, .after = year) |>
  relocate(latitude, .after = county) |> 
  relocate(longitude, .after = latitude) |> 
  relocate(system_type, .after = source_type) |> 
  rename(total_gallons = total)

# convert to long format
waterUse_tidy = waterUse_almostClean |> 
  pivot_longer(cols = Jan:Dec,
               names_to = "month",
               values_to = "gallons")

#------------------------------------------------------------------------------#

# load county population data for merge
load("~/560-Project/clean-data/data/countyPopulations_clean.rds")

# merge waterUse and countyPopulations
waterUse_clean = left_join(waterUse_tidy, countyPopulations,
                            by = c("year", "county"), relationship = "many-to-one")

# convert from population in thousands to actual population, rename, reorder variables, drop source_id (not relevant)
waterUse_clean = waterUse_clean |> 
  mutate(population_thousands = population_thousands*1000) |> 
  rename(population = population_thousands) |> 
  select(
    system_id,
    system_name,
    year,
    county,
    population,
    latitude,
    longitude,
    source_type,
    system_type,
    diversion_type,
    use_type,
    month,
    month_gallons = gallons,
    year_gallons = total_gallons
  )

#------------------------------------------------------------------------------#

# save as .rds file
save(waterUse_clean, file = "waterUse_clean.rds")

#------------------------------------------------------------------------------#

## CLEANING STILL NEEDED ##
# Graph water usage, look for extreme/incorrect values
# Look at observations with 0 total use and see if there are monthly usage records
# Maybe sum up the total column across all 12 months to make sure it's accurate
# Some observations have a nonzero number for total, but zeros for individual months - is this a problem?