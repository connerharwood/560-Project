library(dplyr)
library(tidyverse)
library(skimr)

load("~/560-Project/raw-data/data/precip_raw.rda")

# rename and add columns in Box Elder precip data 
boxelder = precip_boxelder |>
  mutate(year = substr(Date, 1, 4),
         month = substr(Date, 5, 6),
         county = "Box Elder") |>
  select(-Date) |> 
  rename(precipitation = Value)

# rename and add columns in Cache precip data 
cache = precip_cache |>
  mutate(year = substr(Date, 1, 4),
         month = substr(Date, 5, 6),
         county = "Cache") |>
  select(-Date) |> 
  rename(precipitation = Value)

# rename and add columns in Davis precip data 
davis = precip_davis |>
  mutate(year = substr(Date, 1, 4),
         month = substr(Date, 5, 6),
         county = "Davis") |>
  select(-Date) |> 
  rename(precipitation = Value)

# rename and add columns in Juab precip data 
juab = precip_juab |>
  mutate(year = substr(Date, 1, 4),
         month = substr(Date, 5, 6),
         county = "Juab") |>
  select(-Date) |> 
  rename(precipitation = Value)

# rename and add columns in Morgan precip data 
morgan = precip_morgan |>
  mutate(year = substr(Date, 1, 4),
         month = substr(Date, 5, 6),
         county = "Morgan") |>
  select(-Date) |> 
  rename(precipitation = Value)

# rename and add columns in Rich precip data 
rich = precip_rich |>
  mutate(year = substr(Date, 1, 4),
         month = substr(Date, 5, 6),
         county = "Rich") |>
  select(-Date) |> 
  rename(precipitation = Value)

# rename and add columns in Salt Lake precip data 
saltlake = precip_saltlake |>
  mutate(year = substr(Date, 1, 4),
         month = substr(Date, 5, 6),
         county = "Salt Lake") |>
  select(-Date) |> 
  rename(precipitation = Value)

# rename and add columns in Summit precip data 
summit = precip_summit |>
  mutate(year = substr(Date, 1, 4),
         month = substr(Date, 5, 6),
         county = "Summit") |>
  select(-Date) |> 
  rename(precipitation = Value)

# rename and add columns in Tooele precip data 
tooele = precip_tooele |>
  mutate(year = substr(Date, 1, 4),
         month = substr(Date, 5, 6),
         county = "Tooele") |>
  select(-Date) |> 
  rename(precipitation = Value)

# rename and add columns in Utah precip data 
utah = precip_utah |>
  mutate(year = substr(Date, 1, 4),
         month = substr(Date, 5, 6),
         county = "Utah") |>
  select(-Date) |> 
  rename(precipitation = Value)

# rename and add columns in Wasatch precip data 
wasatch = precip_wasatch |>
  mutate(year = substr(Date, 1, 4),
         month = substr(Date, 5, 6),
         county = "Wasatch") |>
  select(-Date) |> 
  rename(precipitation = Value)

# rename and add columns in Weber precip data 
weber = precip_weber |>
  mutate(year = substr(Date, 1, 4),
         month = substr(Date, 5, 6),
         county = "Weber") |>
  select(-Date) |> 
  rename(precipitation = Value)

# append data sets  
precip = rbind(boxelder, cache, davis, juab, morgan, rich, saltlake, summit, tooele, utah, wasatch, weber)

# check structure of precip and missing values
skim(precip)

## No missing values found ## 

# rearrange columns and rename precipitation
precip = precip |> 
  select(year, county, month, precip_in = precipitation) |> 
  mutate(year = as.numeric(year))

# convert month numbers into names 
precip$month = month.abb[as.integer(precip$month)]

# check for duplicates
precip |> 
  n_distinct()

## No duplicates found ## 

# check that all years are present 
all_years = 1959:2023
precip_years = unique(precip$year)
missing_years = setdiff(all_years, precip_years)
print(missing_years)

## No missing years ## 

# save precip data 
precip_clean = precip 
save(precip_clean, file = "precip_clean.rds")
