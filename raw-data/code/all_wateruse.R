library(dplyr)
library(tidyverse)
library(readxl)
library(skimr)
library(sf)
library(sp)

setwd("~/OneDrive - Montana State University/Data Analytics Project Backup/Original Data Files")

wateruse_info = read_csv("water_use_data.csv", skip = 1, col_names = TRUE, n_max = 25241)
wateruse_raw = read_csv("water_use_data.csv", skip = 25244, col_names = TRUE)

setwd("~/560-Project")

#------------------------------------------------------------------------------#
# Step 3: Remove irrelevant, garbage, or empty rows and columns ----

# select & rename relevant variables from wateruse_raw
wateruse3 = wateruse_raw |> 
  select(
    system_name = "System Name",
    system_id = "System ID",
    source_name = "Source Name",
    source_id = "Source ID",
    latitude = "Lat NAD83",
    longitude = "Lon NAD83",
    source_type = "Source Type",
    diversion_type = "Diversion Type",
    use_type = "Use Type",
    year = "Year",
    Jan:Dec,
    total = "Total"
  )

# select & rename relevant variables from wateruse_info
wateruse_info3 = wateruse_info |> 
  select(
    system_id = "System ID",
    system_type = "System Type",
    county = "County"
  )

# check for empty rows or columns in wateruse3 (there are none)
which(nrow(is.na(wateruse3) | wateruse3 == "") == ncol(wateruse3))
which(ncol(is.na(wateruse3) | wateruse3 == "") == nrow(wateruse3))

# check for empty rows or columns in wateruse_info3 (there are none)
which(nrow(is.na(wateruse_info3) | wateruse_info3 == "") == ncol(wateruse_info3))
which(ncol(is.na(wateruse_info3) | wateruse_info3 == "") == nrow(wateruse_info3))

#------------------------------------------------------------------------------#
# Step 5: Resolve duplicates ----

# unique combination of certain variables to identify duplicates in wateruse3
wateruse3 |> 
  count(system_name, system_id, source_name, latitude, longitude, source_type, 
        diversion_type, use_type, year, total) |> 
  filter(n > 1)

# returns same number of rows as wateruse3, so no duplicates
wateruse_distinct = wateruse3 |> 
  distinct()

# unique combination of certain variables to identify duplicates in wateruse_info3
wateruse_info3 |> 
  count(system_id, system_type, county) |> 
  filter(n > 1)

# No duplicates in wateruse3
# Duplicates in wateruse_info3 won't matter since we're just merging with wateruse to get county info

#------------------------------------------------------------------------------#
# Step 8: Understand patterns of missing values ----

# explore waterUse data
skim(wateruse3)

# Ignore missing latitude and longitude values, this seems to be simply lack of info
# We'll still want the water use data from observations missing coordinates

# look at entries with missing year
wateruse_missing_year = wateruse3 |> 
  filter(is.na(year))

# observations missing year have no water use data, so remove these from dataset
wateruse8 = wateruse3 |> 
  filter(!is.na(year))

# explore data again
skim(wateruse8)

# look at entries with missing total_use
wateruse_missing_totaluse = wateruse8 |> 
  filter(is.na(total))

# observations missing total use have no water use data, so remove these from dataset
wateruse8 = wateruse8 |> 
  filter(!is.na(total))

# explore data again
skim(wateruse8)

# NA values for monthly water usage can be assumed to be 0, so change these values to 0
# wateruse8 = wateruse8 |> 
# mutate(across(jan:dec, ~ifelse(is.na(.), 0, .)))

# convert invalid latitude and longitude to NA, then remove NAs
wateruse8 = wateruse8 |> 
  mutate(latitude = ifelse(latitude < 37 | longitude == 0, NA, latitude),
         longitude = ifelse(latitude < 37 | longitude == 0, NA, longitude)) |> 
  filter(!(is.na(latitude)) & !(is.na(longitude)))

# explore data again
skim(wateruse8)

# explore values of each variable in wateruse_info3
skim(wateruse_info3)

# look at entries with missing counties
wateruse_info3 |> 
  filter(is.na(county))

# the entries with missing counties don't seem to be entries for water rights users, so remove these observations
wateruse_info8 = wateruse_info3 |> 
  filter(!is.na(county))

# look at observations with no monthly data to understand those that still have total use values
monthly_zeros = wateruse8 |>
  filter(Jan == 0 &
           Feb == 0 &
           Mar == 0 &
           Apr == 0 &
           May == 0 &
           Jun == 0 &
           Jul == 0 &
           Aug == 0 &
           Sep == 0 &
           Oct == 0 &
           Nov == 0 &
           Dec == 0)
# Going into the Utah water use database to look at some specific water rights users that appear in monthly_zeros,
# it looks like there simply wasn't monthly data recorded, but for some there is a total recorded
# This is mostly with older years and is most likely just a lack of data recorded, so we'll assume the
# yearly usage values are correct and either keep the monthly usage equal to zero, or perhaps take the yearly
# usage number for that observation and assign to each month the monthly average (total use / 12)

#------------------------------------------------------------------------------#
# Step 14: Perform logical checks on quantitative variables ----

# look at unique values of certain character variables for wateruse
wateruse8 |> 
  select(source_type) |> 
  unique()

wateruse8 |> 
  select(diversion_type) |> 
  unique()

wateruse8 |> 
  select(use_type) |> 
  unique()

# look at unique values of certain character variables for wateruse_info
wateruse_info8 |> 
  select(system_type) |> 
  unique()

wateruse_info8 |> 
  select(county) |> 
  unique()

# Relevant character variables look as they should, no need for logical checks

#------------------------------------------------------------------------------#
# Step 15: Clean string variables ----

# see if some use type categories can be combined
wateruse8 |> 
  select(use_type) |> 
  unique()

# combine power use types into one "Power" category, filter out "NA" use type
wateruse15 = wateruse8 |> 
  mutate(
    use_type = ifelse(
      use_type %in% c("Power (Hydro-Elec)", 
                      "Power (Fossil-Fuel)", 
                      "Power (Geothermal)",
                      "Geothermal"), 
      "Power", use_type)) |> 
  filter(!(is.na(use_type)))

#------------------------------------------------------------------------------#
# Merge ----

# create dataset copies for merging
wateruse_merge = wateruse15
wateruse_info_merge = wateruse_info8

# merge waterUse and waterUse_info datasets
wateruse_merged = left_join(wateruse_merge, wateruse_info_merge, 
                            by = "system_id", relationship = "many-to-many")

#------------------------------------------------------------------------------#
# Geospatial ----

# Great Salt Lake Basin shapefile
gsl_basin = st_read("~/560-Project/geospatial/gsl basin/GSLSubbasins.shp")

gsl_basin = st_transform(gsl_basin, crs = st_crs("+proj=longlat +datum=WGS84"))

# Convert water use data to sf object
wateruse_sf <- st_as_sf(wateruse_merged, coords = c("longitude", "latitude"), crs = st_crs(gsl_basin))

# Perform intersection to find points within the basin
within_basin <- st_intersection(wateruse_sf, gsl_basin)