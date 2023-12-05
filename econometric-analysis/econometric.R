library(dplyr)
library(tidyverse)

load("~/560-Project/clean-data/data/masterdata.rds")

#------------------------------------------------------------------------------#
# Aggregate yearly data ----

# GSL yearly average level and volume
gsl_yearly = masterdata |> 
  # convert volume from cubic meters to gallons
  mutate(gsl_volume_gal = gsl_volume_m3 * 264.172052) |> 
  select(year, gsl_level_ft, gsl_volume_gal) |> 
  group_by(year) |> 
  summarize(
    gsl_level_ft = mean(gsl_level_ft),
    gsl_volume_gal = mean(gsl_volume_gal)
  ) |> 
  arrange(year) |> 
  # calculate year-over-year change in level and volume
  mutate(
    gsl_level_change = (gsl_level_ft - lag(gsl_level_ft)) / lag(gsl_level_ft) * 100,
    gsl_volume_change = (gsl_volume_gal - lag(gsl_volume_gal)) / lag(gsl_volume_gal) * 100
  )

# yearly total population across GSL Basin counties
pop_yearly = masterdata |> 
  select(year, county, population_thousands) |> 
  mutate(population = population_thousands * 1000) |> 
  group_by(year, county) |> 
  summarize(
    population = mean(population)
  ) |> 
  group_by(year) |> 
  summarize(
    population = sum(population)
  )

# yearly average precipitation across GSL Basin counties
precip_yearly = masterdata |> 
  select(year, month, county, precip_in) |> 
  group_by(year, month, county) |> 
  summarize(
    precip_in = mean(precip_in)
  ) |> 
  group_by(year, county) |> 
  summarize(
    precip_in = sum(precip_in)
  ) |> 
  group_by(year) |> 
  summarize(
    precip_in = mean(precip_in)
  )

# yearly total water usage per use type
wateruse_yearly = masterdata |> 
  select(year, use_type, year_gallons) |> 
  group_by(year, use_type) |> 
  summarize(
    year_gallons = sum(year_gallons)
  )

# merge yearly datasets into one
yearly_merge1 = left_join(wateruse_yearly, gsl_yearly, by = "year", relationship = "many-to-one")
yearly_merge2 = left_join(yearly_merge1, pop_yearly, by = "year", relationship = "many-to-one")
yearly_merge3 = left_join(yearly_merge2, precip_yearly, by = "year", relationship = "many-to-one")

# yearly usage data for each use type
yearly_per_use = yearly_merge3 |> 
  # calculate yearly per capita water usage for each use type
  mutate(
    percapita_usage = year_gallons / population
  )

# yearly total usage data across all use types
yearly_total_use = yearly_merge3 |> 
  group_by(year) |> 
  summarize(
    year_gallons = sum(year_gallons),
    gsl_level_ft = mean(gsl_level_ft),
    gsl_volume_gal = mean(gsl_volume_gal),
    gsl_level_change = mean(gsl_level_change),
    gsl_volume_change = mean(gsl_volume_change),
    population = mean(population),
    precip_in = mean(precip_in)
  ) |> 
  # calculate yearly per capita water usage
  mutate(
    percapita_usage = year_gallons / population
  )

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
  # calculate year-over-year change in level and volume
  mutate(
    gsl_level_change = (gsl_level_ft - lag(gsl_level_ft)) / lag(gsl_level_ft) * 100,
    gsl_volume_change = (gsl_volume_gal - lag(gsl_volume_gal)) / lag(gsl_volume_gal) * 100
  )

# monthly total population across GSL Basin counties
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
  )

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
  )

# monthly total water usage per use type
wateruse_monthly = masterdata_monthly |>
  filter(month_gallons != 0) |> 
  select(date, use_type, month_gallons) |> 
  group_by(date, use_type) |> 
  summarize(
    month_gallons = sum(month_gallons)
  )

# merge monthly datasets into one
monthly_merge1 = left_join(wateruse_monthly, gsl_monthly, by = "date", relationship = "many-to-one")
monthly_merge2 = left_join(monthly_merge1, pop_monthly, by = "date", relationship = "many-to-one")
monthly_merge3 = left_join(monthly_merge2, precip_monthly, by = "date", relationship = "many-to-one")

# monthly usage data for each use type
monthly_per_use = monthly_merge3 |> 
  # calculate monthly per capita water usage for each use type
  mutate(
    percapita_usage = month_gallons / population
  )

# monthly total usage data across all use types
monthly_total_use = monthly_merge3 |> 
  group_by(date) |> 
  summarize(
    month_gallons = sum(month_gallons),
    gsl_level_ft = mean(gsl_level_ft),
    gsl_volume_gal = mean(gsl_volume_gal),
    gsl_level_change = mean(gsl_level_change),
    gsl_volume_change = mean(gsl_volume_change),
    population = mean(population),
    precip_in = mean(precip_in)
  ) |> 
  # calculate monthly per capita water usage
  mutate(
    percapita_usage = month_gallons / population
  )