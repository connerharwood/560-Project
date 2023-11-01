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
    
  )
