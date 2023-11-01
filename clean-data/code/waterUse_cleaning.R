library(dplyr)
library(tidyverse)

load("~/560-Project/raw-data/data/rawData.rda")

# 
waterUse |> 
  select(County) |> 
  distinct()
waterUse |> 
  group_by(System.Name) |> 
  filter(n() > 1)
waterUse_counties = waterUse |> 
  filter(County == c("Box Elder", "Cache", "Davis", "Morgan", "Rich", "Salt Lake", "Tooele", "Utah", "Weber"))

waterUse_counties |> 
  select(System.Name) |> 
  n_distinct()
