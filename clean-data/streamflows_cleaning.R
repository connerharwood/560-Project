library(dplyr)
library(tidyverse)
library(skimr)

# streamflow gauge at start of Bear River, USGS site number = 10011500
# select and rename relevant variables, separate dateTime column
bearRiver_start_clean = bearRiver_start |> 
  select(-agency_cd, -site_no, -X_00060_00000_cd, -tz_cd) |> 
  rename(start_discharge = X_00060_00000) |> 
  separate(dateTime, into = c("year", "month", "day")) |> 
  mutate(year = as.numeric(year),
         month = as.numeric(month),
         day = as.numeric(day))

# streamflow gauge at end of Bear River, USGS site number = 10126000
# select and rename relevant variables, separate dateTime column
bearRiver_end_clean = bearRiver_end |> 
  select(-agency_cd, -site_no, -X_00060_00000_cd, -tz_cd) |> 
  rename(end_discharge = X_00060_00000) |> 
  separate(dateTime, into = c("year", "month", "day")) |> 
  mutate(year = as.numeric(year),
         month = as.numeric(month),
         day = as.numeric(day))

# streamflow gauge at start of Weber River, USGS site number = 10128500
# select and rename relevant variables, separate dateTime column
weberRiver_start_clean = weberRiver_start |> 
  select(-agency_cd, -site_no, -X_00060_00000_cd, -tz_cd) |> 
  rename(start_discharge = X_00060_00000) |> 
  separate(dateTime, into = c("year", "month", "day")) |> 
  mutate(year = as.numeric(year),
         month = as.numeric(month),
         day = as.numeric(day))

# streamflow gauge at end of Weber River, USGS site number = 10141000
# select and rename relevant variables, separate dateTime column
weberRiver_end_clean = weberRiver_end |> 
  select(-agency_cd, -site_no, -X_00060_00000_cd, -tz_cd) |> 
  rename(end_discharge = X_00060_00000) |> 
  separate(dateTime, into = c("year", "month", "day")) |> 
  mutate(year = as.numeric(year),
         month = as.numeric(month),
         day = as.numeric(day))

# check for missing years
all_years = 1986:2023

# missing years in Bear River starting gauge streamflows
bearStart_years = unique(bearRiver_start_clean$year)
missing_bearStart_years = setdiff(all_years, bearStart_years)
print(missing_bearStart_years)

# missing years in Bear River ending gauge streamflows
bearEnd_years = unique(bearRiver_end_clean$year)
missing_bearEnd_years = setdiff(all_years, bearEnd_years)
print(missing_bearEnd_years)

# missing years in Weber River starting gauge streamflows
weberStart_years = unique(weberRiver_start_clean$year)
missing_weberStart_years = setdiff(all_years, weberStart_years)
print(missing_weberStart_years)

# missing years in Weber River ending gauge streamflows
weberEnd_years = unique(weberRiver_end_clean$year)
missing_weberEnd_years = setdiff(all_years, weberEnd_years)
print(missing_weberEnd_years)