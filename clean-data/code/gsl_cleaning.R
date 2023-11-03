library(dplyr)
library(tidyr)
library(skimr)

load("~/560-Project/raw-data/data/rawData.rda")

# select and rename variables of interest for North Arm of GSL
GSL_northLevels_clean = GSL_northLevels |> 
  select(date = Date, north_levels = X_62614_00003) |> 
  separate(date, into = c("year", "month", "day"))

# select and rename variables of interest for South Arm of GSL
GSL_southLevels_clean = GSL_southLevels |>
  select(date = Date, south_levels = X_62614_00003) |> 
  separate(date, into = c("year", "month", "day"))

# check duplicates
GSL_northLevels_clean |> 
  n_distinct()

# check duplicates
GSL_southLevels_clean |> 
  n_distinct()

## No duplicates in either ##

# check missing values in North Arm
skim(GSL_northLevels_clean)

# check missing values in South Arm
skim(GSL_southLevels_clean)

## No missing values in either ##

## check for missing years ##
all_years = 1966:2023

# missing years in North Arm
GSL_northLevels_years = unique(GSL_northLevels_clean$year)
missing_GSL_northLevels_year = setdiff(all_years, GSL_northLevels_years)
print(missing_GSL_northLevels_year)

# missing years in South Arm
GSL_southLevels_years = unique(GSL_southLevels_clean$year)
missing_GSL_southLevels_year = setdiff(all_years, GSL_southLevels_years)
print(missing_GSL_southLevels_year)

## No missing years in either ##


gsl_levels = merge(GSL_northLevels, GSL_southLevels, by = "Date")

gsl_levels$north_levels = as.numeric(gsl_levels$north_levels)
gsl_levels$south_levels = as.numeric(gsl_levels$south_levels)

gsl_levels = gsl_levels |> 
  mutate(level = (north_levels + south_levels) / 2)

gsl_levels$year = format(gsl_levels$Date, "%Y")

gsl_levels <- aggregate(level ~ year, gsl_levels, FUN = mean)


