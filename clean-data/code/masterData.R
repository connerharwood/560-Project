library(dplyr)
library(tidyverse)

load("~/560-Project/clean-data/data/waterUse_clean.rds")
load("~/560-Project/clean-data/data/gsl_levels_clean.rds")
load("~/560-Project/clean-data/data/precip_clean.rds")
load("~/560-Project/clean-data/data/landUse_clean.rds")
load("~/560-Project/clean-data/data/countyPopulations_clean.rds")

# forgot to convert year in gsl level dataset to numeric, so doing that here
gsl_levels = gsl_levels |> 
  mutate(year = as.numeric(year))

# forgot to convert year in land use dataset to numeric, so doing that here
landUse_clean = landUse_clean |> 
  mutate(year = as.numeric(year))

# aggregate great salt lake levels to yearly average 
gsl_levels = gsl_levels |> 
  group_by(year) |> 
  summarize(gsl_level = mean(level))

# 
population = countyPopulations |> 
  mutate(population = population_thousands*1000) |> 
  group_by(year) |> 
  summarize(population = sum(population))

# merge water use data with GSL levels data by year and month
waterUse_gslLevels = left_join(waterUse_clean, gsl_levels, by = c("year", "month"), relationship = "many-to-one")

# drop irrelevant key variable
waterUse_gslLevels = waterUse_gslLevels |> 
  select(-key)

# calculate total usage per use type category
use_type_total = masterData |> 
  group_by(use_type) |> 
  summarize(use_total = sum(year_gallons)) |> 
  arrange(desc(use_total))

# create a new data table with yearly aggregates 
yearly_data = masterData |> 
  group_by(year, use_type) |> 
  summarize(total_per_use = sum(year_gallons),
            gsl_level = mean(gsl_level, na.rm = TRUE),
            population = mean(population, na.rm = TRUE)) |>
  mutate(ln_total_per_use = log(total_per_use),
         usage_per_capita = total_per_use/population,
         ln_usage_per_capita = log(usage_per_capita)) |> 
  # remove sewage treatment because only data available for a few years and small water user
  filter(year >= 1970 & use_type != "Sewage Treatment") 

# calculate the total water usage per year
yearly_data = yearly_data |> 
  group_by(year) |> 
  mutate(total = sum(total_per_use)) |> 
  ungroup()

# merge water use and GSL levels data with precipitation data by year, county, and month
masterData = left_join(waterUse_gslLevels, precip_clean, by = c("year", "county", "month"), relationship = "many-to-one")

# reorder and rename some variables
masterData = masterData |> 
  relocate(month, .after = year) |> 
  rename(gsl_level = level,
         county_precip = precip_in)

save(masterData, file = "masterData.rds")
