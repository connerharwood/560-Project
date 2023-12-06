library(dplyr)
library(tidyverse)
library(zoo)

load("~/560-Project/clean-data/data/masterdata.rds")

# create aggregated data at monthly level by use type for regression model

#------------------------------------------------------------------------------#
# Aggregate monthly data ----

# convert month and year to date format
masterdata_monthly = masterdata |> 
  mutate(
    date = as.Date(paste(year, month, "01"), format = "%Y %b %d"),
    date = as.yearmon(date)
  )

# GSL monthly level and volume
gsl_monthly = masterdata_monthly |> 
  # convert volume from cubic meters to gallons
  mutate(
    gsl_volume_gal = gsl_volume_m3 * 264.172052
  ) |> 
  select(date, gsl_level_ft, gsl_volume_gal) |> 
  group_by(date) |> 
  summarize(
    gsl_level_ft = mean(gsl_level_ft),
    gsl_volume_gal = mean(gsl_volume_gal)
  ) |> 
  arrange(date) |> 
  # calculate year-over-year change in level and volume for each month
  mutate(
    gsl_level_change = gsl_level_ft - lag(gsl_level_ft, 12),
    gsl_volume_change = gsl_volume_gal - lag(gsl_volume_gal, 12)
  ) |> 
  # filter out 1996, which has NAs for change variables
  filter(date >= 1997)

# yearly total population across GSL Basin counties, assigned to each month for the regression
pop_monthly = masterdata_monthly |> 
  select(date, county, population_thousands) |> 
  mutate(population = population_thousands * 1000) |> 
  group_by(date, county) |> 
  summarize(
    population = mean(population)
  ) |> 
  group_by(date) |> 
  summarize(
    population = sum(population)
  ) |> 
  filter(date >= 1997)

# monthly average precipitation across GSL Basin counties
precip_monthly = masterdata_monthly |> 
  select(date, county, precip_in) |> 
  group_by(date, county) |> 
  summarize(
    precip_in = mean(precip_in)
  ) |> 
  group_by(date) |> 
  summarize(
    precip_in = mean(precip_in)
  ) |>
  # calculate year-over-year change in precipitation for each month
  mutate(precip_change = precip_in - lag(precip_in, 12)) |> 
  # filter out 1996, which has NAs for precip_change
  filter(date >= 1997)

# monthly total water usage per use type
wateruse_monthly = masterdata_monthly |>
  filter(month_gallons != 0) |> 
  select(date, use_type, month_gallons) |> 
  group_by(date, use_type) |> 
  summarize(
    month_gallons = sum(month_gallons)
  ) |> 
  group_by(use_type) |> 
  arrange(use_type, date) |> 
  # calculate year-over-year change in water use for each month
  mutate(
    month_gallons_change = month_gallons - lag(month_gallons, 12)
  ) |>
  # filter out 1996, which has NAs for month_gallons_change
  filter(date >= 1997) |> 
  mutate(month_gallons_change = ifelse(is.na(month_gallons_change), 0, month_gallons_change))

# merge monthly datasets into one
monthly_merge1 = left_join(wateruse_monthly, gsl_monthly, by = "date", relationship = "many-to-one")
monthly_merge2 = left_join(monthly_merge1, pop_monthly, by = "date", relationship = "many-to-one")
monthly_merge3 = left_join(monthly_merge2, precip_monthly, by = "date", relationship = "many-to-one")

reg_data = monthly_merge3

save(reg_data, file = "reg_data.rds")

#------------------------------------------------------------------------------#
# 1996-2022 ----

# for robustness, include years 2015-2022 in the regression

load("~/560-Project/clean-data/data/wateruse_1996_2022.rds")
load("~/560-Project/clean-data/data/gsl_clean.rds")
load("~/560-Project/clean-data/data/precip_clean.rds")
load("~/560-Project/clean-data/data/population_clean.rds")

# merge water use and precipitation data
merge1 = left_join(wateruse_1996_2022, precip_clean, by = c("year", "month", "county"), relationship = "many-to-one")

# merge water use and precipitation data with GSL data
merge2 = left_join(merge1, gsl_clean, by = c("year", "month"), relationship = "many-to-one")

# merge water use, precipitation, and GSL data with population data
merge3 = left_join(merge2, population_clean, by = c("year", "county"), relationship = "many-to-one")

# change merge3 to data table
merge3 = as.data.table(merge3)

# select, reorder, and rename relevant variables
merge4 = merge3 |> 
  mutate(key = row_number()) |> 
  select(
    key,
    system_id:longitude,
    precip_in,
    gsl_level_ft = level,
    gsl_volume_m3 = volume_m3,
    population_thousands
  )

masterdata_1996_2022 = merge4

#------------------------------------------------------------------------------#
# Aggregate 1996-2022 monthly data ----

# convert month and year to date format
masterdata_1996_2022_2 = masterdata_1996_2022 |> 
  mutate(
    date = as.Date(paste(year, month, "01"), format = "%Y %b %d"),
    date = as.yearmon(date)
  )

# monthly total water usage per use type
wateruse_1996_2022_monthly = masterdata_1996_2022_2 |>
  filter(month_gallons != 0) |> 
  select(date, use_type, month_gallons) |> 
  group_by(date, use_type) |> 
  summarize(
    month_gallons = sum(month_gallons)
  ) |> 
  group_by(use_type) |> 
  arrange(use_type, date) |> 
  mutate(
    month_gallons_change = month_gallons - lag(month_gallons, 12)
  ) |>
  filter(date >= 1997) |> 
  mutate(month_gallons_change = ifelse(is.na(month_gallons_change), 0, month_gallons_change))

# merge monthly datasets into one
all_monthly_merge1 = left_join(wateruse_1996_2022_monthly, gsl_monthly, by = "date", relationship = "many-to-one")
all_monthly_merge2 = left_join(all_monthly_merge1, pop_monthly, by = "date", relationship = "many-to-one")
all_monthly_merge3 = left_join(all_monthly_merge2, precip_monthly, by = "date", relationship = "many-to-one")

reg_1996_2022 = all_monthly_merge3

#------------------------------------------------------------------------------#
# save ----

save(reg_1996_2022, file = "reg_1996_2022.rds")

#------------------------------------------------------------------------------#
# create new dataset with total use by each use type for 1996-2022 ---- 

reg_type_1996_2022 = reg_1996_2022 |> 
  group_by(use_type) |>
  mutate(value = month_gallons_change) |>
  ungroup() |>
  spread(use_type, value, fill = 0) 

reg_type_1996_2022 <- reg_type_1996_2022 |>
  rename(water_supplier = "Water Supplier") |> 
  group_by(date, gsl_level_ft, gsl_volume_gal, gsl_level_change, gsl_volume_change, population, precip_in, precip_change) |> 
  summarise(agricultural = sum(Agricultural),
            commercial = sum(Commercial), 
            domestic = sum(Domestic),
            industrial = sum(Industrial), 
            irrigation = sum(Irrigation), 
            power = sum(Power), 
            water_supplier = sum(water_supplier))

#------------------------------------------------------------------------------#
# save ----

save(reg_type_1996_2022, file = "reg_type_1996_2022.rds")

#------------------------------------------------------------------------------#
# create new dataset with total use by each use type for 1996-2014 ---- 

reg_type_1996_2014 = reg_data |> 
  group_by(use_type) |>
  mutate(value = month_gallons_change) |>
  ungroup() |>
  spread(use_type, value, fill = 0) 

reg_type_1996_2014 <- reg_type_1996_2014 |>
  rename(water_supplier = "Water Supplier") |> 
  group_by(date, gsl_level_ft, gsl_volume_gal, gsl_level_change, gsl_volume_change, population, precip_in, precip_change) |>
  summarise(agricultural = sum(Agricultural),
            commercial = sum(Commercial), 
            domestic = sum(Domestic),
            industrial = sum(Industrial), 
            irrigation = sum(Irrigation), 
            power = sum(Power), 
            water_supplier = sum(water_supplier))

#------------------------------------------------------------------------------#
# save ----

save(reg_type_1996_2014, file = "reg_type_1996_2014.rds")
