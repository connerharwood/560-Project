library(tidyverse)
library(dplyr)
library(skimr)
library(scales)
library(summarytools)

load("~/560-Project/raw-data/data/rawData.rda")

# rename and select relevant variables from 1986-1992 dataset
landUse1986_1992_clean = landUse1986_1992 |> 
  rename(id = ï..FID,
         county = COUNTY,
         basin = BASIN_NAME,
         year = SURV_YEAR,
         description = DESCRIPTIO,
         land_use = LANDUSE,
         acres = ACRES
         ) |> 
  select(-BASIN, 
         -COUNTYNBR, 
         -LANDCOV, 
         -OldDescrip,
         -LUID, 
         -SHAPE_Length, 
         -SHAPE_Area)

# rename and select relevant variables from 1989-1999 dataset
landUse1989_1999_clean = landUse1989_1999 |> 
  rename(id = ï..FID,
         county = COUNTY,
         basin = BASIN_NAME,
         year = SURV_YEAR,
         description = DESCRIPTIO,
         land_use = LANDUSE,
         acres = ACRES
         ) |> 
  select(-BASIN, 
         -COUNTYNBR, 
         -LANDCOV, 
         -OldDescrip,
         -LUID, 
         -SHAPE_Length, 
         -SHAPE_Area)

# rename and select relevant variables from 2000-2005 dataset
landUse2000_2005_clean = landUse2000_2005 |> 
  rename(id = ï..OBJECTID,
         county = COUNTY,
         basin = BASIN_NAME,
         year = SURV_YEAR,
         description = DESCRIPTIO,
         land_use = LANDUSE,
         acres = ACRES
         ) |> 
  select(-BASIN, 
         -COUNTYNBR, 
         -LANDCOV, 
         -IRR_TYPE, 
         -LABEL, 
         -OldLabel, 
         -OldDescrip,
         -LUID, 
         -Shape__Area, 
         -Shape__Length)

# rename and select relevant variables from 2005-2010 dataset
landUse2005_2010_clean = landUse2005_2010 |> 
  rename(id = OBJECTID,
         county = COUNTY,
         basin = BASIN_NAME,
         year = SURV_YEAR,
         description = DESCRIPTIO,
         land_use = LANDUSE,
         acres = ACRES
         ) |> 
  select(-ï..OBJECTID_1,
         -BASIN, 
         -COUNTYNBR, 
         -LANDCOV, 
         -IRR_TYPE, 
         -LABEL, 
         -OldLabel, 
         -OldDescrip,
         -LUID, 
         -Shape__Area, 
         -Shape__Length)

# rename and select relevant variables from 2010-2015 dataset
landUse2010_2015_clean = landUse2010_2015 |> 
  rename(id = OBJECTID,
         county = COUNTY,
         basin = BASIN_NAME,
         year = SURV_YEAR,
         description = DESCRIPTIO,
         land_use = LANDUSE,
         acres = ACRES
         ) |> 
  select(-ï..OBJECTID_12,
         -OBJECTID_1,
         -BASIN, 
         -COUNTYNBR, 
         -LANDCOV, 
         -IRR_TYPE, 
         -LABEL,
         -Shape_Leng,
         -Shape__Area, 
         -Shape__Length)

# rename and select relevant variables from 2016 dataset
landUse2016_clean = landUse2016 |> 
  rename(id = OBJECTID,
         county = COUNTY,
         basin = BASIN_NAME,
         year = SURV_YEAR,
         description = DESCRIPTIO,
         land_use = LANDUSE,
         acres = ACRES
         ) |> 
  select(-OBJECTID_1,
         -LABEL,
         -IRR_TYPE,
         -CLASS_NAME,
         -LANDCOV,
         -CropType,
         -BASIN,
         -COUNTYNBR,
         -COORDINATE,
         -STEWARD,
         -Shape_Leng,
         -Shape_Le_1,
         -Shape_Area)

# rename and select relevant variables from 2017 dataset
landUse2017_clean = landUse2017 |> 
  rename(id = LUID,
         county = County,
         basin = Basin,
         year = SURV_YEAR,
         description = Description,
         land_use = Landuse,
         acres = Acres
         ) |> 
  select(-ï..OBJECTID,
         -OBJECTID_1,
         -CropGroup,
         -IRR_Method,
         -State,
         -SubArea,
         -Label_Class,
         -LABEL,
         -Class_Name,
         -OldLanduse,
         -LU_Group,
         -Shape__Area,
         -Shape__Length)

# rename and select relevant variables from 2018 dataset
landUse2018_clean = landUse2018 |> 
  rename(id = LUID,
         county = County,
         basin = Basin,
         year = SURV_YEAR,
         description = Description,
         land_use = Landuse,
         acres = Acres
         ) |> 
  select(-ï..OBJECTID,
         -OBJECTID_1,
         -CropGroup,
         -IRR_Method,
         -State,
         -SubArea,
         -Label_Class,
         -LABEL,
         -Class_Name,
         -OldLanduse,
         -LU_Group,
         -SHAPE_Area,
         -SHAPE_Length)

# rename and select relevant variables from 2019 dataset
landUse2019_clean = landUse2019 |> 
  rename(id = ï..OBJECTID,
         county = County,
         basin = Basin,
         year = SURV_YEAR,
         description = Description,
         land_use = Landuse,
         acres = Acres
         ) |> 
  select(-CropGroup,
         -IRR_Method,
         -State,
         -SubArea,
         -Label_Class,
         -LABEL,
         -Class_Name,
         -OldLanduse,
         -LU_Group,
         -Shape__Area,
         -Shape__Length)

# rename and select relevant variables from 2020 dataset
landUse2020_clean = landUse2020 |> 
  rename(id = ï..OBJECTID,
         county = County,
         basin = Basin,
         year = SURV_YEAR,
         description = Description,
         land_use = Landuse,
         acres = Acres
         ) |> 
  select(-CropGroup,
         -IRR_Method,
         -State,
         -SubArea,
         -Label_Class,
         -LABEL,
         -Class_Name,
         -OldLanduse,
         -LU_Group,
         -SHAPE_Area,
         -SHAPE_Length)

# rename and select relevant variables from 2021 dataset
landUse2021_clean = landUse2021 |> 
  rename(id = ï..OBJECTID,
         county = County,
         basin = Basin,
         year = SURV_YEAR,
         description = Description,
         land_use = Landuse,
         acres = Acres
         ) |> 
  select(-CropGroup,
         -IRR_Method,
         -State,
         -SubArea,
         -Label_Class,
         -LABEL,
         -Class_Name,
         -OldLanduse,
         -LU_Group,
         -SHAPE_Length,
         -SHAPE_Area)

# rename and select relevant variables from 2022 dataset
landUse2022_clean = landUse2022 |> 
  rename(id = ï..OBJECTID,
         county = County,
         basin = Basin,
         year = SURV_YEAR,
         description = Description,
         land_use = Landuse,
         acres = Acres
         ) |> 
  select(-CropGroup,
         -IRR_Method,
         -State,
         -SubArea,
         -Label_Class,
         -LABEL,
         -Class_Name,
         -OldLanduse,
         -LU_Group,
         -SHAPE_Length,
         -SHAPE_Area)

# combine all water-related land use datasets into one dataset
landUse_combined = rbind(landUse1986_1992_clean, 
                         landUse1989_1999_clean, 
                         landUse2000_2005_clean,
                         landUse2005_2010_clean, 
                         landUse2010_2015_clean, 
                         landUse2016_clean, 
                         landUse2017_clean, 
                         landUse2018_clean,
                         landUse2019_clean, 
                         landUse2020_clean, 
                         landUse2021_clean,
                         landUse2022_clean)

# save combined land use dataframe as file
save(landUse_combined, file = "landUse_combined.rds")

## Cleaning Checklist

# 1, 2, 3, 7, 9, 10, 11, 12, 13: not necessary or already completed

# 4: Identify the primary key, or define a surrogate key

# id not a key variable, so make surrogate key and remove id:
landUse_clean1 = landUse_combined |> 
  select(-id) |> 
  mutate(key = 1:nrow(landUse_combined)) |> 
  select(key, everything())

# 5: Resolve duplicates

# keep roles with distinct combinations of all variables (except surrogate key variable)
landUse_clean2 = landUse_clean1 |> 
  distinct(county, basin, year, description, land_use, acres, .keep_all = TRUE)

# 6: Understand the definition, origin, and units of each variable

# look at distinct values of land_use variable
landUse_clean2 |> 
  select(land_use) |> 
  distinct()

# 8: Understand patterns of missing values
skim(landUse_clean2)

# set whitespace values in land_use to NA
landUse_clean3 = landUse_clean2 |> 
  mutate(land_use = ifelse(land_use == " ", NA, land_use))

# check land_use = X description 
landUse_clean3 |>
  filter(land_use == "X")
# we remove these observations in a later step 

# 14: Perform logical checks on quantitative variables

# 15: Clean string variables

# convert county and basin variables to Title Format, squish any whitespaces
landUse_clean4 = landUse_clean3 |> 
  mutate(across(c(county, basin, description, land_use), squish),
         county = str_to_title(county),
         basin = str_to_title(basin),
         description = str_to_title(description),
         land_use = toupper(land_use))

## Various additonal cleaning steps as needed

# check distinct values for counties
landUse_clean4 |> 
  select(county) |> 
  distinct()

# select only counties in Great Salt Lake Basin, recreate surrogate key
landUse_clean5 = landUse_clean4 |> 
  filter(county == c("Box Elder", "Cache", "Davis", "Morgan", "Rich", "Salt Lake", "Tooele", "Utah", "Weber"),
         land_use != "X")

# remove duplicates after fixing inconsistent spellings
landUse_clean6 = landUse_clean5 |> 
  distinct(county, basin, year, land_use, acres, .keep_all = TRUE)

# drop NA values since land with no use not relevant to us
landUse_clean = landUse_clean6 |> 
  na.omit()

# redefine surrogate key
landUse_clean = landUse_clean |> 
  mutate(key = 1:nrow(landUse_clean))

# check land_use categories 
landUse_clean |> 
  select(land_use) |>
  distinct()

# combine same land_use categories with different labels into one label
landUse_clean = landUse_clean |> 
  mutate(land_use = ifelse(land_use %in% c("URBAN", "URB"), "URBAN", land_use),
         land_use = ifelse(land_use %in% c("RIPARIAN/WETLAND", "RIP"), "RIPARIAN/WETLAND", land_use))

save(landUse_clean, file = "landUse_clean.rds")
