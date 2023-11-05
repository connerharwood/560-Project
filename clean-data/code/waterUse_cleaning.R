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

# look at observations with no monthly data to understand those that still have total use values
monthly_zeros = waterUse8 |>
  filter(Jan == 0 &
           Feb == 0 &
           Mar == 0 &
           Apr == 0 &
           May == 0 &
           Jun == 0 &
           Jul == 0 &
           Aug == 0 &
           Sep == 0 &
           Oct == 0 &
           Nov == 0 &
           Dec == 0)
# Going into the Utah water use database to look at some specific water rights users appear in monthly_zeros,
# it looks like there simply wasn't monthly data recorded, but for some there is a total recorded
# This is mostly with older years and is most likely just a lack of data recorded, so we'll assume the
# yearly usage values are correct and either keep the monthly usage equal to zero, or perhaps take the yearly
# usage number for that observation and assign to each month the monthly average (total use / 12)

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


monthly_zeros = waterUse_almostClean |>
  filter(Jan == 0 &
           Feb == 0 &
           Mar == 0 &
           Apr == 0 &
           May == 0 &
           Jun == 0 &
           Jul == 0 &
           Aug == 0 &
           Sep == 0 &
           Oct == 0 &
           Nov == 0 &
           Dec == 0)

# convert to long format
waterUse_tidy = waterUse_almostClean |> 
  pivot_longer(cols = Jan:Dec,
               names_to = "month",
               values_to = "gallons")

#------------------------------------------------------------------------------#

# load county population data for merge
load("~/560-Project/clean-data/data/countyPopulations_clean.rds")

# merge waterUse and countyPopulations
waterUse_clean1 = left_join(waterUse_tidy, countyPopulations,
                            by = c("year", "county"), relationship = "many-to-one")

# convert from population in thousands to actual population, rename, reorder variables, drop source_id (not relevant)
waterUse_clean1 = waterUse_clean1 |> 
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

# drop observations that have no monthly or yearly water use data
waterUse_clean2 = waterUse_clean1 |> 
  filter(year_gallons != 0)

# looking at the data, there are two observations with much higher usage than the rest of the data,
# and the water use data base is not showing these records, so these may have been an error caused during import
# Thus, we'll be removing these two observations below

# define surrogate key to remove the 2 observations
waterUse_clean_rm = waterUse_clean2 |> 
  mutate(key = 1:nrow(waterUse_clean2))

# remove the two invalid observations
waterUse_clean_rm = waterUse_clean_rm |> 
  filter(key != 332034 & key != 332035)

waterUse_clean3 = waterUse_clean_rm

# calculate what months use the most water as a percentage of the total
totals = waterUse_clean3 |>
  group_by(month) |> 
  summarize(monthly_total = sum(month_gallons, na.rm = TRUE)) |> 
  mutate(total = sum(monthly_total),
         month_percent = monthly_total / total)

# extract monthly percentages from totals dataframe
jan = totals[totals$month == "Jan", ]
jan_percent = jan$month_percent

feb = totals[totals$month == "Feb", ]
feb_percent = feb$month_percent

mar = totals[totals$month == "Mar", ]
mar_percent = mar$month_percent

apr = totals[totals$month == "Apr", ]
apr_percent = apr$month_percent

may = totals[totals$month == "May", ]
may_percent = may$month_percent

jun = totals[totals$month == "Jun", ]
jun_percent = jun$month_percent

jul = totals[totals$month == "Jul", ]
jul_percent = jul$month_percent

aug = totals[totals$month == "Aug", ]
aug_percent = aug$month_percent

sep = totals[totals$month == "Sep", ]
sep_percent = sep$month_percent

oct = totals[totals$month == "Oct", ]
oct_percent = oct$month_percent

nov = totals[totals$month == "Nov", ]
nov_percent = nov$month_percent

dec = totals[totals$month == "Dec", ]
dec_percent = dec$month_percent

total = totals$total
total = total[1]

# impute monthly averages for observations missing all months
waterUse_impute = waterUse_clean3 |> 
  group_by(year_gallons) |> 
  mutate(month_gallons = ifelse(all(month_gallons == 0) & month == "Jan", jan_percent*year_gallons,
         ifelse(all(month_gallons == 0) & month == "Feb", feb_percent*year_gallons,
         ifelse(all(month_gallons == 0) & month == "Mar", mar_percent*year_gallons,
         ifelse(all(month_gallons == 0) & month == "Apr", apr_percent*year_gallons,
         ifelse(all(month_gallons == 0) & month == "May", may_percent*year_gallons,
         ifelse(all(month_gallons == 0) & month == "Jun", jun_percent*year_gallons,
         ifelse(all(month_gallons == 0) & month == "Jul", jul_percent*year_gallons,
         ifelse(all(month_gallons == 0) & month == "Aug", aug_percent*year_gallons,
         ifelse(all(month_gallons == 0) & month == "Sep", sep_percent*year_gallons,
         ifelse(all(month_gallons == 0) & month == "Oct", oct_percent*year_gallons,
         ifelse(all(month_gallons == 0) & month == "Nov", nov_percent*year_gallons,
         ifelse(all(month_gallons == 0) & month == "Dec", dec_percent*year_gallons, month_gallons)))))))))))))

# look at observations with no monthly data to check a random one (system_id == 1008 is what we chose)
check = waterUse_clean3 |> 
  group_by(year_gallons) |> 
  filter(all(month_gallons == 0))

# check system_id = 1008 before imputing
sys1008 = waterUse_impute |> 
  filter(system_id == 1008 & year == 1988)

# check system_id = 1008 after imputing
sys1008_2 = waterUse_clean3 |> 
  filter(system_id == 1008 & year == 1988)

## IT WORKED, WE ARE BETTER THAN EVERY DATA SCIENTIST EVER ##
#------------------------------------------------------------------------------#

waterUse_clean = waterUse_impute

# save as .rds file
save(waterUse_clean, file = "waterUse_clean.rds")