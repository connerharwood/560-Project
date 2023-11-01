library(dplyr)
library(tidyverse)

load("~/560-Project/raw-data/data/waterUse_raw.rda")

#------------------------------------------------------------------------------#

## Data cleaning checklist ##

# 1: Convert file formats
# Already done

# 2: Import data and wrangle into a tidy layout
# Already done

# 3: Remove irrelevant, garbage, or empty rows and columns

# select relevant variables from waterUse_raw
waterUse3 = waterUse_raw |> 
  select(
    "System Name",
    "System ID",
    "System Type",
    "History Year",
    "County",
    "Population",
    "Domestic Use",  
    "Commercial Use",
    "Industrial Use",
    "Institutional Use",
    "Total Use",
    "Method of Measurement",
    "Secondary Metered Domestic Use",
    "Secondary Metered Commercial Use",
    "Secondary Metered Industrial Use",
    "Secondary Metered Institutional Use",
    "Secondary Metered Agriculture Use"
    )

# select relevant variables from waterUse_info
waterUse_info3 = waterUse_info |> 
  select(
    "System Name",
    "System ID",
    "Lat NAD83",
    "Lon NAD83",
    "Source Type",
    "Diversion Type",
    "Use Type",
    "Year",
    "Units",
    "Jan",
    "Feb",
    "Mar",
    "Apr",
    "May",
    "Jun",
    "Jul",
    "Aug",
    "Sep",
    "Oct",
    "Nov",
    "Dec",
    "Total"
  )
