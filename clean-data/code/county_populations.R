library(dplyr)
library(tidyverse)
library(readxl)
library(lubridate)

# county resident populations obtained from here: https://fred.stlouisfed.org/categories/30154

# read in 1970-2022 resident population for the 9 Utah counties of interest
countyPopulations = read_xls("county_populations.xls", sheet = "Annual", col_names = TRUE)

# rename columns
countyPopulations = countyPopulations |> 
  rename(
    "Box Elder" = UTBOXE3POP,
    "Cache" = UTCACH0POP,
    "Davis" = UTDAVI0POP,
    "Morgan" = UTMORG9POP,
    "Rich" = UTRICH3POP,
    "Salt Lake" = UTSALT5POP,
    "Tooele" = UTTOOE5POP,
    "Utah" = UTUTAH9POP,
    "Weber" = UTWEBE7POP
  )

# pivot into long format
countyPopulations = countyPopulations |> 
  pivot_longer(cols = "Box Elder":"Weber", 
               names_to = "county", 
               values_to = "population_thousands")

# extract just year from DATE column, remove DATE column, move year column to front
countyPopulations = countyPopulations |> 
  mutate(year = year(DATE)) |> 
  select(-DATE) |> 
  relocate(year)

# save as .rds file
save(countyPopulations, file = "countyPopulations_clean.rds")
