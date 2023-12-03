library(dplyr)
library(tidyverse)
library(data.table)

load("~/560-Project/clean-data/data/wateruse_clean.rds")
load("~/560-Project/clean-data/data/gsl_clean.rds")
load("~/560-Project/clean-data/data/precip_clean.rds")
load("~/560-Project/clean-data/data/population_clean.rds")

#------------------------------------------------------------------------------#
# merge ----

# merge water use and precipitation data
merge1 = left_join(wateruse_clean, precip_clean, by = c("year", "month", "county"), relationship = "many-to-one")

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

#------------------------------------------------------------------------------#
# save ----

masterdata = merge4

save(masterdata, file = "masterdata.rds")
