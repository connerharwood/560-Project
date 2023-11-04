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

# look at data structure
str(GSL_northLevels_clean)
str(GSL_southLevels_clean)

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

# calculate average monthly levels for each year in North Arm
GSL_northLevels_monthly = GSL_northLevels_clean |> 
  group_by(year, month) |> 
  summarize(north_levels = mean(north_levels))

# calculate average monthly levels for each year in South Arm
GSL_southLevels_monthly = GSL_southLevels_clean |> 
  group_by(year, month) |> 
  summarize(south_levels = mean(south_levels))

# merge North and South levels
gsl_levels = merge(GSL_northLevels_monthly, GSL_southLevels_monthly, by = c("year", "month"))

## February 1967 not an observation in merged dataset ##

# calculate average between North and South levels for each month, drop North and South levels
gsl_levels = gsl_levels |> 
  mutate(level = (north_levels + south_levels) / 2) |> 
  select(-north_levels, -south_levels)

# convert month numbers to character month names
gsl_levels$month = month.abb[as.integer(gsl_levels$month)]

save(gsl_levels, file = "gsl_levels_clean.rds")
