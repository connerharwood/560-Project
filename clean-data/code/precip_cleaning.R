library(dplyr)
library(tidyverse)

# remove irrelevant columns and rename 
precip_clean1 = precip_raw |> 
  select(-...1, -meanprecip, -diff, -percent, -sd) |> 
  rename(total = tot, average = ave)

# convert data to long format 
precip_clean2 = precip_clean1 |>
  pivot_longer(cols = "jan":"dec",
               names_to = "month",
               values_to = "precip")

# rearrange and rename columns 
precip_clean2 = precip_clean2 |> 
  select(fips, 
         year, 
         month, 
         precip_in = precip, 
         year_total = total, 
         year_avg = average, 
         year_drought = drought)

# keep fip codes of interest
precip_clean3 = precip_clean2 |> 
  mutate(county = ifelse(fips == "49003", "Box Elder",
         ifelse(fips == "49005", "Cache",
         ifelse(fips == "49011", "David",
         ifelse(fips == "49029", "Morgan",
         ifelse(fips == "49033", "Rich",
         ifelse(fips == "49035", "Salt Lake",
         ifelse(fips == "49045", "Tooele",
         ifelse(fips == "49049", "Utah",
         ifelse(fips == "49057", "Weber", NA)))))))))) 

is_present = any(grepl("^25", precip_raw$fips))

fips_county = data.frame(
  county = c("Box Elder", "Cache", "David", "Morgan", "Rich", "Salt Lake", "Tooele", "Utah", "Weber"), 
  fips = c("49003", "49005", "49011", "49029", "49033", "49035", "49045", "49049", "49057")
)

library(scales)

precip_squish = precip_clean2 |>
  squish(fips)

precip_fips = left_join(precip_clean2, fips_county, relationship = "many-to-one")

precip_fips = precip_fips |>
  filter(!is.na(county))

str(precip_raw$fips)
  

precip_clean3 |>
  filter(!is.numeric(fips))


