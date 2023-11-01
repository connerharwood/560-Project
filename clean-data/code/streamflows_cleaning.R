library(dplyr)
library(tidyverse)
library(ggplot2)

load("~/560-Project/raw-data/data/rawData.rda")

#------------------------------------------------------------------------------#

# streamflow gauge at start of Bear River, USGS site number = 10011500
# select and rename relevant variables, separate dateTime column
bearStart_clean = bearRiver_start |>
  select(-agency_cd, -site_no, -X_00060_00000_cd, -tz_cd) |> 
  rename(start_discharge = X_00060_00000) |> 
  separate(dateTime, into = c("year", "month", "day")) |> 
  mutate(year = as.numeric(year),
         month = as.numeric(month),
         day = as.numeric(day)) |> 
  filter(year > 1986 & year < 2023) # filter out incomplete years o 1986 and 2023

# streamflow gauge at end of Bear River, USGS site number = 10126000
# select and rename relevant variables, separate dateTime column
bearEnd_clean = bearRiver_end |> 
  select(-agency_cd, -site_no, -X_00060_00000_cd, -tz_cd) |> 
  rename(end_discharge = X_00060_00000) |> 
  separate(dateTime, into = c("year", "month", "day")) |> 
  mutate(year = as.numeric(year),
         month = as.numeric(month),
         day = as.numeric(day)) |> 
  filter(year > 1986 & year < 2023) # filter out incomplete years o 1986 and 2023

# streamflow gauge at start of Weber River, USGS site number = 10128500
# select and rename relevant variables, separate dateTime column
weberStart_clean = weberRiver_start |> 
  select(-agency_cd, -site_no, -X_00060_00000_cd, -tz_cd) |> 
  rename(start_discharge = X_00060_00000) |> 
  separate(dateTime, into = c("year", "month", "day"), remove = FALSE) |> 
  mutate(year = as.numeric(year),
         month = as.numeric(month),
         day = as.numeric(day)) |> 
  filter(year > 1986 & year < 2023) # filter out incomplete years o 1986 and 2023

# streamflow gauge at end of Weber River, USGS site number = 10141000
# select and rename relevant variables, separate dateTime column
weberEnd_clean = weberRiver_end |> 
  select(-agency_cd, -site_no, -X_00060_00000_cd, -tz_cd) |> 
  rename(end_discharge = X_00060_00000) |> 
  separate(dateTime, into = c("year", "month", "day")) |> 
  mutate(year = as.numeric(year),
         month = as.numeric(month),
         day = as.numeric(day)) |> 
  filter(year > 1986 & year < 2023) # filter out incomplete years o 1986 and 2023

#------------------------------------------------------------------------------#

# check for missing years
all_years = 1987:2022

# missing years in Bear River starting gauge streamflows
bearStart_years = unique(bearStart_clean$year)
missing_bearStart_years = setdiff(all_years, bearStart_years)
print(missing_bearStart_years)

# missing years in Bear River ending gauge streamflows
bearEnd_years = unique(bearEnd_clean$year)
missing_bearEnd_years = setdiff(all_years, bearEnd_years)
print(missing_bearEnd_years)

# missing years in Weber River starting gauge streamflows
weberStart_years = unique(weberStart_clean$year)
missing_weberStart_years = setdiff(all_years, weberStart_years)
print(missing_weberStart_years)

# missing years in Weber River ending gauge streamflows
weberEnd_years = unique(weberEnd_clean$year)
missing_weberEnd_years = setdiff(all_years, weberEnd_years)
print(missing_weberEnd_years)

# check number of months with observations in each year
weberStart_nmonths = weberStart_clean |> 
  group_by(year) |> 
  summarize(n_months = n_distinct(month))

#------------------------------------------------------------------------------#

## impute missing months as average discharge of corresponding month for preceding and following year ##

# calculate yearly streamflow averages for weberStart
weberStart_yearly_monthly = weberStart_clean |>
  group_by(year, month) |> 
  summarize(startDischarge_average = mean(start_discharge))

# create dataset with all combinations of years and months
all_months1 = expand.grid(
  year = unique(weberStart_yearly_monthly$year),
  month = 1:12
)

# merge monthly averages per year dataset with year-month combo set for imputing
weberStart_all_months = all_months1 |> 
  left_join(weberStart_yearly_monthly, by = c("year", "month"))

# impute monthly averages for missing values (mutate didn't work but transform did)
weberStart_imputed = transform(
  weberStart_all_months,
  startDischarge_imputed = ifelse(is.na(startDischarge_average),
                                 (lag(startDischarge_average, default = NA) + 
                                  lead(startDischarge_average, default = NA)) / 2,
                                  startDischarge_average))

#------------------------------------------------------------------------------#

# repeat imputing process for weberEnd, bearStart, and bearEnd

# calculate yearly streamflow averages for weberEnd
weberEnd_yearly_monthly = weberEnd_clean |>
  group_by(year, month) |> 
  summarize(endDischarge_average = mean(end_discharge))

# create dataset with all combinations of years and months
all_months2 = expand.grid(
  year = unique(weberEnd_yearly_monthly$year),
  month = 1:12
)

# merge monthly averages per year dataset with year-month combo set for imputing
weberEnd_all_months = all_months2 |> 
  left_join(weberEnd_yearly_monthly, by = c("year", "month"))

# impute monthly averages for missing values (mutate didn't work but transform did)
weberEnd_imputed = transform(
  weberEnd_all_months,
  endDischarge_imputed = ifelse(is.na(endDischarge_average),
                                  (lag(endDischarge_average, default = NA) + 
                                   lead(endDischarge_average, default = NA)) / 2,
                                   endDischarge_average))

#------------------------------------------------------------------------------#

# calculate yearly streamflow averages for bearStart
bearStart_yearly_monthly = bearStart_clean |>
  group_by(year, month) |> 
  summarize(startDischarge_average = mean(start_discharge))

# create dataset with all combinations of years and months
all_months3 = expand.grid(
  year = unique(bearStart_yearly_monthly$year),
  month = 1:12
)

# merge monthly averages per year dataset with year-month combo set for imputing
bearStart_all_months = all_months3 |> 
  left_join(bearStart_yearly_monthly, by = c("year", "month"))

# impute monthly averages for missing values (mutate didn't work but transform did)
bearStart_imputed = transform(
  bearStart_all_months,
  startDischarge_imputed = ifelse(is.na(startDischarge_average),
                                  (lag(startDischarge_average, default = NA) + 
                                     lead(startDischarge_average, default = NA)) / 2,
                                  startDischarge_average))

#------------------------------------------------------------------------------#

# calculate yearly streamflow averages for bearEnd
bearEnd_yearly_monthly = bearEnd_clean |>
  group_by(year, month) |> 
  summarize(endDischarge_average = mean(end_discharge))

# create dataset with all combinations of years and months
all_months4 = expand.grid(
  year = unique(bearEnd_yearly_monthly$year),
  month = 1:12
)

# merge monthly averages per year dataset with year-month combo set for imputing
bearEnd_all_months = all_months4 |> 
  left_join(bearEnd_yearly_monthly, by = c("year", "month"))

# impute monthly averages for missing values (mutate didn't work but transform did)
bearEnd_imputed = transform(
  bearEnd_all_months,
  endDischarge_imputed = ifelse(is.na(endDischarge_average),
                                (lag(endDischarge_average, default = NA) + 
                                   lead(endDischarge_average, default = NA)) / 2,
                                endDischarge_average))

#------------------------------------------------------------------------------#

# check months with the most missing values for weberStart
weberStart_imputed |> 
  group_by(month) |> 
  summarize(missing_values = sum(is.na(startDischarge_imputed)))

# check months with the most missing values for weberEnd
weberEnd_imputed |> 
  group_by(month) |> 
  summarize(missing_values = sum(is.na(endDischarge_imputed)))

# check months with the most missing values for bearStart
bearStart_imputed |> 
  group_by(month) |> 
  summarize(missing_values = sum(is.na(startDischarge_imputed)))

# check months with the most missing values for bearEnd
bearEnd_imputed |> 
  group_by(month) |> 
  summarize(missing_values = sum(is.na(endDischarge_imputed)))

# There are many years without January, February, or December observations, even after imputing,
# for weberStart and bearStart (likely because these gauges are at winter-influenced sites in the mountains)
# These months typically have the lowest streamflows, so I'll exclude them since yearly averages calculated
# without these months will be skewed higher than years averaged with these months
# I'll exclude them for all 4 datasets to level their comparisons

#------------------------------------------------------------------------------#

# exclude January, February, and December from weberStart
weberStart_imputed = weberStart_imputed |> 
  filter(month != 1 & month != 2 & month != 12)

# exclude January, February, and December from weberEnd
weberEnd_imputed = weberEnd_imputed |> 
  filter(month != 1 & month != 2 & month != 12)

# exclude January, February, and December from bearStart
bearStart_imputed = bearStart_imputed |> 
  filter(month != 1 & month != 2 & month != 12)

# exclude January, February, and December from bearEnd
bearEnd_imputed = bearEnd_imputed |> 
  filter(month != 1 & month != 2 & month != 12)

#------------------------------------------------------------------------------#

# compute yearly averages for all 4 datasets

# yearly average streamflows for weberStart
weberStart = weberStart_imputed |> 
  group_by(year) |> 
  summarize(weberStart_discharge = mean(startDischarge_imputed, na.rm = TRUE))

# yearly average streamflows for weberEnd
weberEnd = weberEnd_imputed |> 
  group_by(year) |> 
  summarize(weberEnd_discharge = mean(endDischarge_imputed, na.rm = TRUE)) |> 
  rename(year2 = year) # rename year for cbind

# yearly average streamflows for bearStart
bearStart = bearStart_imputed |> 
  group_by(year) |> 
  summarize(bearStart_discharge = mean(startDischarge_imputed, na.rm = TRUE)) |> 
  rename(year3 = year) # rename year for cbind

# yearly average streamflows for bearEnd
bearEnd = bearEnd_imputed |> 
  group_by(year) |> 
  summarize(bearEnd_discharge = mean(endDischarge_imputed, na.rm = TRUE)) |> 
  rename(year4 = year) # rename year for cbind

#------------------------------------------------------------------------------#

# combine Weber and Bear streamflows into one dataset by year
streamflows_clean = cbind(weberStart, weberEnd, bearStart, bearEnd)

# drop duplicate year columns
streamflows_clean = streamflows_clean |> 
  select(-year2, -year3, -year4)

# save streamflows dataset
save(streamflows_clean, file = "streamflows_clean.rds")
