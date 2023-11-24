library(dplyr)
library(tidyverse)
library(skimr)
library(lubridate)

load("~/560-Project/raw-data/data/raw_data.rda")

# select and rename variables of interest for North Arm of GSL
gsl_north_levels_clean = gsl_north_levels |> 
  select(date = Date, north_levels = X_62614_00003) |> 
  separate(date, into = c("year", "month", "day"))

# select and rename variables of interest for South Arm of GSL
gsl_south_levels_clean = gsl_south_levels |>
  select(date = Date, south_levels = X_62614_00003) |> 
  separate(date, into = c("year", "month", "day"))

# look at data structure
str(gsl_north_levels_clean)
str(gsl_south_levels_clean)

# check duplicates
gsl_north_levels_clean |> 
  n_distinct()

# check duplicates
gsl_south_levels_clean |> 
  n_distinct()

## No duplicates in either ##

# check missing values in North Arm
skim(gsl_north_levels_clean)

# check missing values in South Arm
skim(gsl_south_levels_clean)

## No missing values in either ##

## check for missing years ##
all_years = 1966:2023

# missing years in North Arm
gsl_north_levels_years = unique(gsl_north_levels_clean$year)
missing_gsl_north_levels_year = setdiff(all_years, gsl_north_levels_years)
print(missing_gsl_north_levels_year)

# missing years in South Arm
gsl_south_levels_years = unique(gsl_south_levels_clean$year)
missing_gsl_south_levels_year = setdiff(all_years, gsl_south_levels_years)
print(missing_gsl_south_levels_year)

## No missing years in either ##

# calculate average monthly levels for each year in North Arm
gsl_north_levels_monthly = gsl_north_levels_clean |> 
  group_by(year, month) |> 
  summarize(north_levels = mean(north_levels))

# calculate average monthly levels for each year in South Arm
gsl_south_levels_monthly = gsl_south_levels_clean |> 
  group_by(year, month) |> 
  summarize(south_levels = mean(south_levels))

# merge North and South levels
gsl_levels = merge(gsl_north_levels_monthly, gsl_south_levels_monthly, by = c("year", "month"))

## February 1967 not an observation in merged dataset ##

# calculate average between North and South levels for each month, drop North and South levels
gsl_levels = gsl_levels |> 
  mutate(level = (north_levels + south_levels) / 2,
         year = as.numeric(year)) |> 
  select(-north_levels, -south_levels)

# convert month numbers to character month names
gsl_levels$month = month.abb[as.integer(gsl_levels$month)]

# load GSL volume data
load("~/560-Project/raw-data/data/gsl_volume_raw.rds")

# select relevant variables
gsl_volume = gsl_volume_raw |> 
  select(date = Date,
         volume_m3 = Total_vol_m3)

# separate date column into year and month columns for merge, reorder
gsl_volume = gsl_volume |> 
  mutate(
    year = year(date),
    month = month(date)
  ) |> 
  select(year, month, volume_m3)

# convert month numbers to character month names
gsl_volume$month = month.abb[as.integer(gsl_volume$month)]

# merge volume data with levels data
gsl_merged = left_join(gsl_levels, gsl_volume, by = c("year", "month"), relationship = "one-to-one")

# drop missing month of October 2023
gsl_merged = na.omit(gsl_merged)

# rename
gsl = gsl_merged

save(gsl, file = "gsl_clean.rds")
