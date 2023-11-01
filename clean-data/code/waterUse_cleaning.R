library(dplyr)
library(tidyverse)
library(skimr)

load("~/560-Project/raw-data/data/waterUse_raw.rda")

#------------------------------------------------------------------------------#

# I learned later in the cleaning process that waterUse_info is the main water use dataset we're interested in,
# and waterUse_raw has some info we'll want to merge into waterUse_info
# To be more clear with names, I'll be using the format waterUse for the original waterUse_info dataset, 
# and waterUse_info for the original waterUse_raw dataset
# The numbers in dataframe names indicate the data cleaning step that dataset was created in

## Data cleaning checklist ##

# 1: Convert file formats
# Already done

# 2: Import data and wrangle into a tidy layout
# Already done

# 3: Remove irrelevant, garbage, or empty rows and columns

# select relevant variables from waterUse_raw
waterUse_info3 = waterUse_raw |> 
  select(
    "System Name",
    "System ID",
    "System Type",
    "History Year",
    "County",
    "Population",
    "Domestic Use",  
    "Commercial Use",
    "Industrial Use",
    "Institutional Use",
    "Total Use",
    "Secondary Metered Domestic Use",
    "Secondary Metered Commercial Use",
    "Secondary Metered Industrial Use",
    "Secondary Metered Institutional Use",
    "Secondary Metered Agriculture Use"
    )

# select relevant variables from waterUse_info
waterUse3 = waterUse_info |> 
  select(
    "System Name",
    "System ID",
    "Source Name",
    "Source ID",
    "Source Status",
    "Lat NAD83",
    "Lon NAD83",
    "Source Type",
    "Diversion Type",
    "Use Type",
    "Year",
    "Jan",
    "Feb",
    "Mar",
    "Apr",
    "May",
    "Jun",
    "Jul",
    "Aug",
    "Sep",
    "Oct",
    "Nov",
    "Dec",
    "Total"
  )

# check for empty rows or columns in waterUse_info3 (there are none)
which(nrow(is.na(waterUse_info3) | waterUse_info3 == "") == ncol(waterUse_info3))
which(ncol(is.na(waterUse_info3) | waterUse_info3 == "") == nrow(waterUse_info3))

# check for empty rows or columns in waterUse3 (there are none)
which(nrow(is.na(waterUse3) | waterUse3 == "") == ncol(waterUse3))
which(ncol(is.na(waterUse3) | waterUse3 == "") == nrow(waterUse3))

# 4: Identify the primary key, or define a surrogate key

# Not necessary at this stage, will do after merging the two dataframes

# 5: Resolve duplicates

# unique combination of certain variables to identify duplicates in waterUse_info3
waterUse_info3 |> 
  count(`System ID`, `History Year`, `Total Use`) |> 
  filter(n > 1)

# unique combination of certain variables to identify duplicates in waterUse_info3
waterUse3 |> 
  count(`System Name`, `System ID`, `Lat NAD83`, `Lon NAD83`, `Source Type`, 
        `Diversion Type`, `Use Type`, `Year`, `Total`) |> 
  filter(n > 1)

# Looks like any duplicates are due to NA values, which will be resolved in a later cleaning step

# 6: Understand the definition, origin, and units of each variable

# Most are self explanatory
# All water usage expressed in gallons
# Will understand secondary usage variables in later stage

# 7: Rename variables as necessary

# rename variables in waterUse_info
waterUse_info7 = waterUse_info3 |> 
  rename(
    system_name = "System Name",
    system_id = "System ID",
    system_type = "System Type",
    year = "History Year",
    county = "County",
    population = "Population",
    domestic_use = "Domestic Use",
    commercial_use = "Commercial Use",
    industrial_use = "Industrial Use",
    institutional_use = "Institutional Use",
    total_use = "Total Use",
    secondary_domestic_use = "Secondary Metered Domestic Use",
    secondary_commercial_use = "Secondary Metered Commercial Use",
    secondary_industrial_use = "Secondary Metered Industrial Use",
    secondary_institutional_use = "Secondary Metered Institutional Use",
    secondary_agriculture_use = "Secondary Metered Agriculture Use"
  )

# rename variables in waterUse
waterUse7 = waterUse3 |> 
  rename(
    system_name = "System Name",
    system_id = "System ID",
    source_name = "Source Name",
    source_id = "Source ID",
    source_status = "Source Status",
    latitude = "Lat NAD83",
    longitude = "Lon NAD83",
    source_type = "Source Type",
    diversion_type = "Diversion Type",
    use_type = "Use Type",
    year = "Year",
    jan = "Jan",
    feb = "Feb",
    mar = "Mar",
    apr = "Apr",
    may = "May",
    jun = "Jun",
    jul = "Jul",
    aug = "Aug",
    sep = "Sep",
    oct = "Oct",
    nov = "Nov",
    dec = "Dec",
    total_use = "Total"
  )

# 8: Understand patterns of missing values

# explore values of each variable in waterUse_info7
skim(waterUse_info7)

# look at entries with missing counties
waterUse_info_missingCounty = waterUse_info7 |> 
  filter(is.na(county))

# the entries with missing counties don't seem to be entries for water rights users, so remove these observations
waterUse_info8 = waterUse_info7 |> 
  filter(!is.na(county))

# explore missing values again
skim(waterUse_info8)

# look at entries with missing population
waterUse_info_missingPopulation = waterUse_info8 |> 
  filter(is.na(population))

# Populations don't seem to be correct for a lot of years, so ignore for now and perhaps later merge with
# correct population data from separate data source

# look at entries with missing total_use
waterUse__info_missingTotalUse = waterUse_info8 |> 
  filter(is.na(total_use))

# After looking at missing total_use values for waterUse_info8, I've realized the waterUse dataset has
# the most complete info, so this will be what we'll primarily use moving forward
# We'll use the waterUse_info dataset to merge counties and system type

# select variables we want to merge from waterUse_info
waterUse_info8_merge = waterUse_info8 |> 
  select(system_id, system_type, county)

#------------------------------------------------------------------------------#

# Clean waterUse_info to get ready for merge with waterUse

# 8: Understand patterns of missing values

# explore waterUse data
skim(waterUse7)

# ignore missing latitude and longitude values, this seems to be simply lack of info
# We'll still want these observations

# look at entries with missing year
waterUse_missingYear = waterUse7 |> 
  filter(is.na(year))

# observations missing year have no water use data, so remove these from dataset
waterUse8 = waterUse7 |> 
  filter(!is.na(year))

# explore data again
skim(waterUse8)

# look at entries with missing total_use
waterUse_missingTotalUse = waterUse8 |> 
  filter(is.na(total_use))

# observations missing total use have no water use data, so remove these from dataset
waterUse8 = waterUse8 |> 
  filter(!is.na(total_use))

# explore data again
skim(waterUse8)

# NA values for monthly water usage can be assumed to be 0, so change these values to 0
waterUse8_rm_na = waterUse8 |> 
  mutate(across(jan:dec, ~ifelse(is.na(.), 0, .)))

skim(waterUse8_rm_na)
skim(waterUse_info8_merge)

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

# 14: Perform logical checks on quantitative variables

# look at unique values of certain character variables for waterUse
waterUse8_rm_na |> 
  select(source_status) |> 
  unique()

waterUse8_rm_na |> 
  select(source_type) |> 
  unique()

waterUse8_rm_na |> 
  select(diversion_type) |> 
  unique()

waterUse8_rm_na |> 
  select(use_type) |> 
  unique()

# look at unique values of certain character variables for waterUse_info
waterUse_info8_merge |> 
  select(system_type) |> 
  unique()

waterUse_info8_merge |> 
  select(county) |> 
  unique()

# Character variables look as they should

# 15: Clean string variables

# use_type has two categories that could maybe be combined: "Geothermal" and "Power (Geothermal)"
waterUse8_rm_na |> 
  select(use_type) |> 
  unique()

# look at observations with use type of "Power (Geothermal)" and "Geothermal"
waterUse15_geothermal = waterUse8_rm_na |> 
  filter(use_type == c("Power (Geothermal)", "Geothermal"))

waterUse_merge = waterUse8_rm_na
waterUse_info_merge = waterUse_info8_merge

# The 4 observations "Power (Geothermal)" have no water use data, so I'll ignore this for now
# We may also remove observations without 0 water use later

#------------------------------------------------------------------------------#

# merge waterUse and waterUse_info datasets
waterUse_merged = left_join(waterUse_merge, waterUse_info_merge, 
                             by = "system_id", relationship = "many-to-many")

# drop duplicates, reorder columns, drop source_name
waterUse_clean = waterUse_merged |> 
  distinct() |>
  select(-source_name) |> 
  relocate(source_id, .after = system_id) |> 
  relocate(year, .after = source_id) |>
  relocate(county, .after = year) |>
  relocate(latitude, .after = county) |> 
  relocate(longitude, .after = latitude) |> 
  relocate(system_type, .after = source_type) |> 
  rename(total_gallons = total_use)

skim(waterUse_clean)

# save as .rds file
save(waterUse_clean, file = "waterUse_clean.rds")
