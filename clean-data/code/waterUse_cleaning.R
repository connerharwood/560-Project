library(dplyr)
library(tidyverse)

load("~/560-Project/raw-data/data/waterUse_raw.rda")

#------------------------------------------------------------------------------#

## Data cleaning checklist ##

# 1: Convert file formats
# Already done

# 2: Import data and wrangle into a tidy layout

# Use rbind to append waterUse_county dataframes
waterUse_raw = rbind(waterUse_boxelder, waterUse_cache, waterUse_davis, waterUse_morgan,
                     waterUse_rich, waterUse_saltlake, waterUse_tooele, waterUse_utah,
                     waterUse_weber)

# Use rbind to append waterUse_county_info dataframes
waterUse_info = rbind(waterUse_boxelder_info, waterUse_cache_info, waterUse_davis_info, waterUse_morgan_info,
                      waterUse_rich_info, waterUse_saltlake_info, waterUse_tooele_info, waterUse_utah_info,
                      waterUse_weber_info)

# 3: Remove irrelevant, garbage, or empty rows and columns

# select relevant variables from waterUse_raw
waterUse3 = waterUse_raw |> 
  