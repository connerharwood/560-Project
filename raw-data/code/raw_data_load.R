library(dataRetrieval)
library(dplyr)
library(tidyverse)
library(readxl)

# set appropriate working directory (IN CONSOLE) to "Original Data Files" subfolder within shared OneDrive folder

#------------------------------------------------------------------------------#
# GSL data ----

# Great Salt Lake North Arm daily levels
gsl_north_levels = readNWISdv(
  siteNumbers = "10010100", # gauge near Saline, UT
  parameterCd = "62614", # lake elevation parameter code
  startDate = "1966-04-15",
  endDate = ""
)

# Great Salt Lake South Arm daily levels
gsl_south_levels = readNWISdv(
  siteNumbers = "10010000", # gauge at Saltair Boat Harbor
  parameterCd = "62614", # lake elevation parameter code
  startDate = "1966-04-15",
  endDate = ""
)

# read in csv file with monthly Great Salt Lake volume data
gsl_volume_raw = read_csv("gsl_volume.csv", col_names = TRUE)

#------------------------------------------------------------------------------#
# precipitation data ----

precip_boxelder = read_csv("precip_boxElder.csv", skip = 3, col_names = TRUE)
precip_cache = read_csv("precip_Cache.csv", skip = 3, col_names = TRUE)
precip_davis = read_csv("precip_Davis.csv", skip = 3, col_names = TRUE)
precip_morgan = read_csv("precip_Morgan.csv", skip = 3, col_names = TRUE)
precip_rich = read_csv("precip_Rich.csv", skip = 3, col_names = TRUE)
precip_saltlake = read_csv("precip_SaltLake.csv", skip = 3, col_names = TRUE)
precip_tooele = read_csv("precip_Tooele.csv", skip = 3, col_names = TRUE)
precip_utah = read_csv("precip_Utah.csv", skip = 3, col_names = TRUE)
precip_weber = read_csv("precip_Weber.csv", skip = 3, col_names = TRUE)

#------------------------------------------------------------------------------#
# water use data ----

# Load Utah water use data downloaded from here:
# https://waterrights.utah.gov/asp_apps/generalWaterUse/WaterUseList.asp

# Each .csv file has another table of data with different columns starting after the n_max line
# This data contains the water use info we're interested in, as well as info on type of use and latitude/longitude, 
# and is stored in the wateruse_county files
# The wateruse_county_info files contain county and system type, which will be merged with wateruse_county

# Box Elder County
wateruse_boxelder_info = read_csv("wateruse_boxelder.csv", skip = 1, col_names = TRUE, n_max = 1064)
wateruse_boxelder = read_csv("wateruse_boxelder.csv", skip = 1067, col_names = TRUE)

# Cache County
wateruse_cache_info = read_csv("wateruse_cache.csv", skip = 1, col_names = TRUE, n_max = 1467)
wateruse_cache = read_csv("wateruse_cache.csv", skip = 1470, col_names = TRUE)

# Davis County
wateruse_davis_info = read_csv("wateruse_davis.csv", skip = 1, col_names = TRUE, n_max = 1114)
wateruse_davis = read_csv("wateruse_davis.csv", skip = 1117, col_names = TRUE)

# Morgan County
wateruse_morgan_info = read_csv("wateruse_morgan.csv", skip = 1, col_names = TRUE, n_max = 462)
wateruse_morgan = read_csv("wateruse_morgan.csv", skip = 465, col_names = TRUE)

# Rich County
wateruse_rich_info = read_csv("wateruse_rich.csv", skip = 1, col_names = TRUE, n_max = 284)
wateruse_rich = read_csv("wateruse_rich.csv", skip = 287, col_names = TRUE)

# Salt Lake County
wateruse_saltlake_info = read_csv("wateruse_saltlake.csv", skip = 1, col_names = TRUE, n_max = 3086)
wateruse_saltlake = read_csv("wateruse_saltlake.csv", skip = 3089, col_names = TRUE)

# Tooele County
wateruse_tooele_info = read_csv("wateruse_tooele.csv", skip = 1, col_names = TRUE, n_max = 1309)
wateruse_tooele = read_csv("wateruse_tooele.csv", skip = 1312, col_names = TRUE)

# Utah County
wateruse_utah_info = read_csv("wateruse_utah.csv", skip = 1, col_names = TRUE, n_max = 2566)
wateruse_utah = read_csv("wateruse_utah.csv", skip = 2569, col_names = TRUE)

# Weber County
wateruse_weber_info = read_csv("wateruse_weber.csv", skip = 1, col_names = TRUE, n_max = 1178)
wateruse_weber = read_csv("wateruse_weber.csv", skip = 1181, col_names = TRUE)

# Use rbind to append wateruse_county dataframes
wateruse_raw = rbind(wateruse_boxelder, wateruse_cache, wateruse_davis, wateruse_morgan,
                     wateruse_rich, wateruse_saltlake, wateruse_tooele, wateruse_utah,
                     wateruse_weber)

# Use rbind to append wateruse_county_info dataframes
wateruse_info = rbind(wateruse_boxelder_info, wateruse_cache_info, wateruse_davis_info, wateruse_morgan_info,
                      wateruse_rich_info, wateruse_saltlake_info, wateruse_tooele_info, wateruse_utah_info,
                      wateruse_weber_info)

#------------------------------------------------------------------------------#
# population data ----

# county resident populations obtained from here: https://fred.stlouisfed.org/categories/30154

# read in 1970-2022 resident population for the 9 Utah counties of interest
population_raw = read_xls("county_populations.xls", sheet = "Annual", col_names = TRUE)

#------------------------------------------------------------------------------#
# save data ----

# save all raw data to R data file
save(gsl_north_levels,  
     gsl_south_levels,
     gsl_volume_raw,
     precip_boxelder,
     precip_cache,
     precip_davis,
     precip_morgan,
     precip_rich,
     precip_saltlake,
     precip_tooele,
     precip_utah,
     precip_weber,
     wateruse_raw,
     wateruse_info,
     population_raw,
     file = "raw_data.rda"
)

# save GSL raw data
save(gsl_north_levels,
     gsl_south_levels,
     gsl_volume_raw,
     file = "gsl_raw.rda")

# save precipitation raw data
save(precip_boxelder,
     precip_cache,
     precip_davis,
     precip_morgan,
     precip_rich,
     precip_saltlake,
     precip_tooele,
     precip_utah,
     precip_weber,
     file = "precip_raw.rda")

# save water use raw data
save(wateruse_raw,
     wateruse_info,
     file = "wateruse_raw.rda")

# save population raw data
save(population_raw,
     file = "population_raw.rda")