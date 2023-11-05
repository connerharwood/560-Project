library(dplyr)
library(tidyverse)

hello = "hello"

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
masterData2= masterData |> 
  mutate(use_type = ifelse(use_type == "Irrigation", "Agricultural",
                    ifelse(use_type %in% c("Power (Hydro-Elec)", 
                                           "Geothermal", 
                                           "Power (Fossil-Fuel)", 
                                           "Power (Geothermal)"),
                           "Power", use_type)))

# check to see if regrouping categories worked
unique_use_type2 = unique(masterData2$use_type)
unique_use_type2

#------------------------------------------------------------------------------#

# calculate total usage per use type category
use_type_total = masterData2 |> 
  group_by(use_type) |> 
  summarize(use_total = sum(year_gallons)) |> 
  arrange(desc(use_total))

