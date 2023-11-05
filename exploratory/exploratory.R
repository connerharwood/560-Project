library(dplyr)
library(tidyverse)
library(ggplot2)
library(data.table)

# idea for GSL levels graph: plot line at 4198 feet, showing minimum level needed for a healthy lake

load("~/560-Project/clean-data/data/masterData.rds")
load("~/560-Project/clean-data/data/landUse_clean.rds")

#------------------------------------------------------------------------------#

## preparation and some additional cleaning to the master data ##

# remove duplicates in the masterData
masterData = masterData |> 
  distinct()

# look at use type categories
unique_use_type = unique(masterData$use_type)
unique_use_type

# group similar use type categories
masterData = masterData |> 
  mutate(use_type = ifelse(use_type == "Irrigation", "Agricultural",
                    ifelse(use_type %in% c("Power (Hydro-Elec)", 
                                           "Power (Fossil-Fuel)", 
                                           "Power (Geothermal)",
                                           "Geothermal"),
                           "Power", use_type)))

# check to see if regrouping categories worked
unique_use_type2 = unique(masterData$use_type)
unique_use_type2

# look at Cargill Salt Inc.'s 2015 water use 
cargill = masterData |> 
  filter(system_id == 2168 & year == 2015 & source_type == "Lake") 

# remove Cargill Salt Inc. invalid observation for 2015 
masterData = as.data.table(masterData)
masterData = masterData |> 
  mutate(key = 1:nrow(masterData))

# remove invalid Cargill observations from master dataset
masterData = masterData |>
  filter(!(key %in% 331530:331539)) |> 
  select(-key)

#------------------------------------------------------------------------------#

# calculate total usage per use type category
use_type_total = masterData |> 
  group_by(use_type) |> 
  summarize(use_total = sum(year_gallons)) |> 
  arrange(desc(use_total))

# create a graph showing yearly water use totals per use type over time 
ggplot(masterData, aes(x = year, y = log(year_gallons), color = use_type)) +
  stat_summary(fun = sum, geom = "line") +
  labs(title = "Yearly Total Gallons by Water Use Type", x = "Year", y = "Total Gallons") + 
  theme_minimal()

# create a graph showing water use totals relative to GSL water levels 
ggplot() +
  geom_line(data = masterData, aes(x = year, y = gsl_level, color = "GSL Level")) +
  geom_line(data = masterData, aes(x = year, y = log(year_gallons), color = "Log Yearly Water Use Total")) +
  scale_y_continuous(
    sec.axis = sec_axis(~ . * , name = "Log Yearly Water Use Total"),
    name = "GSL Level"
  ) +
  labs(
    title = "GSL Water Levels and Water Use Totals Over Time",
    x = "Year"
  ) +
  theme_minimal() +
  scale_color_manual(values = c("GSL Level" = "green", "Log Yearly Water Use Total" = "red"))
