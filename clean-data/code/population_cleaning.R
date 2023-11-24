library(dplyr)
library(tidyverse)
library(lubridate)

load("~/560-Project/raw-data/data/population_raw.rda")

# rename columns
population1 = population_raw |> 
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
population2 = population1 |> 
  pivot_longer(cols = "Box Elder":"Weber", 
               names_to = "county", 
               values_to = "population_thousands")

# extract just year from DATE column, remove DATE column, move year column to front
population3 = population2 |> 
  mutate(year = year(DATE)) |> 
  select(-DATE) |> 
  relocate(year)

# rename
population_clean = population3

# save as .rds file
save(population_clean, file = "population_clean.rds")
