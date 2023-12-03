library(dataRetrieval)
library(dplyr)
library(tidyverse)
library(readxl)

setwd("~/OneDrive - Montana State University/Data Analytics Project Backup/Original Data Files")

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

# obtained from here: 
# https://www.ncei.noaa.gov/access/monitoring/climate-at-a-glance/county/time-series/UT-023/pcp/all/1/1959-2023

precip_boxelder = read_csv("precip_boxelder.csv", skip = 3, col_names = TRUE)
precip_cache = read_csv("precip_cache.csv", skip = 3, col_names = TRUE)
precip_davis = read_csv("precip_davis.csv", skip = 3, col_names = TRUE)
precip_juab = read_csv("precip_juab.csv", skip = 3, col_names = TRUE)
precip_morgan = read_csv("precip_morgan.csv", skip = 3, col_names = TRUE)
precip_rich = read_csv("precip_rich.csv", skip = 3, col_names = TRUE)
precip_saltlake = read_csv("precip_saltlake.csv", skip = 3, col_names = TRUE)
precip_summit = read_csv("precip_summit.csv", skip = 3, col_names = TRUE)
precip_tooele = read_csv("precip_tooele.csv", skip = 3, col_names = TRUE)
precip_utah = read_csv("precip_utah.csv", skip = 3, col_names = TRUE)
precip_wasatch = read_csv("precip_wasatch.csv", skip = 3, col_names = TRUE)
precip_weber = read_csv("precip_weber.csv", skip = 3, col_names = TRUE)

#------------------------------------------------------------------------------#
# water use data ----

# load Utah water use data downloaded from here:
# https://waterrights.utah.gov/asp_apps/generalWaterUse/WaterUseList.asp

# The .csv file has another table of data with different columns starting after the n_max line
# This data contains the water use info we're interested in, as well as info on type of use and latitude/longitude
# The wateruse_info file contains county and system type and will be merged with wateruse_data

wateruse_info = read_csv("wateruse_data.csv", skip = 1, col_names = TRUE, n_max = 25241)
wateruse_data = read_csv("wateruse_data.csv", skip = 25244, col_names = TRUE)

#------------------------------------------------------------------------------#
# population data ----

# county resident populations obtained from here: 
# https://fred.stlouisfed.org/categories/30154

# read in 1970-2022 resident population for the 12 Utah counties of interest
population_raw = read_xls("county_populations.xls", sheet = "Annual", col_names = TRUE)

#------------------------------------------------------------------------------#
# save data ----

setwd("~/560-Project")

# save all raw data to R data file
save(gsl_north_levels,  
     gsl_south_levels,
     gsl_volume_raw,
     precip_boxelder,
     precip_cache,
     precip_davis,
     precip_juab,
     precip_morgan,
     precip_rich,
     precip_saltlake,
     precip_summit,
     precip_tooele,
     precip_utah,
     precip_wasatch,
     precip_weber,
     wateruse_data,
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
     precip_juab,
     precip_morgan,
     precip_rich,
     precip_saltlake,
     precip_summit,
     precip_tooele,
     precip_utah,
     precip_wasatch,
     precip_weber,
     file = "precip_raw.rda")

# save water use raw data
save(wateruse_data,
     wateruse_info,
     file = "wateruse_raw.rda")

# save population raw data
save(population_raw,
     file = "population_raw.rda")
