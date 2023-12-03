library(dplyr)
library(tidyverse)
library(skimr)

load("~/560-Project/raw-data/data/wateruse_raw.rda")

#------------------------------------------------------------------------------#
# Step 3: Remove irrelevant, garbage, or empty rows and columns ----

# select & rename relevant variables from wateruse_raw
wateruse1 = wateruse_raw |> 
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
wateruse_info1 = wateruse_info |> 
  select(
    system_id = "System ID",
    system_type = "System Type",
    county = "County"
  )

# check for empty rows or columns in wateruse3 (there are none)
which(nrow(is.na(wateruse1) | wateruse1 == "") == ncol(wateruse1))
which(ncol(is.na(wateruse1) | wateruse1 == "") == nrow(wateruse1))

# check for empty rows or columns in wateruse_info3 (there are none)
which(nrow(is.na(wateruse_info1) | wateruse_info1 == "") == ncol(wateruse_info1))
which(ncol(is.na(wateruse_info1) | wateruse_info1 == "") == nrow(wateruse_info1))

#------------------------------------------------------------------------------#
# Step 5: Resolve duplicates ----

# unique combination of certain variables to identify duplicates in wateruse3
wateruse1 |> 
  count(system_name, system_id, source_name, latitude, longitude, source_type, 
        diversion_type, use_type, year, total) |> 
  filter(n > 1)

# returns same number of rows as wateruse3, so no duplicates
wateruse_distinct = wateruse1 |> 
  distinct()

# unique combination of certain variables to identify duplicates in wateruse_info3
wateruse_info1 |> 
  count(system_id, system_type, county) |> 
  filter(n > 1)

# No duplicates in wateruse3
# Duplicates in wateruse_info3 won't matter since we're just merging with wateruse to get county info

#------------------------------------------------------------------------------#
# Step 8: Understand patterns of missing values ----

# explore waterUse data
skim(wateruse1)

# Ignore missing latitude and longitude values, this seems to be simply lack of info
# We'll still want the water use data from observations missing coordinates

# look at entries with missing year
wateruse_missing_year = wateruse1 |> 
  filter(is.na(year))

# observations missing year have no water use data, so remove these from dataset
wateruse2 = wateruse1 |> 
  filter(!is.na(year))

# explore data again
skim(wateruse2)

# look at entries with missing total_use
wateruse_missing_totaluse = wateruse2 |> 
  filter(is.na(total))

# observations missing total use have no water use data, so remove these from dataset
wateruse8 = wateruse2 |> 
  filter(!is.na(total))

# explore data again
skim(wateruse2)

# NA values for monthly water usage can be assumed to be 0, so change these values to 0
# wateruse2 = wateruse2 |> 
 # mutate(across(jan:dec, ~ifelse(is.na(.), 0, .)))

# convert invalid latitude and longitude to NA
wateruse3 = wateruse2 |> 
  mutate(latitude = ifelse(latitude < 37 | longitude == 0, NA, latitude),
         longitude = ifelse(latitude < 37 | longitude == 0, NA, longitude))

# explore data again
skim(wateruse3)

# explore values of each variable in wateruse_info3
skim(wateruse_info1)

# look at entries with missing counties
wateruse_info1 |> 
  filter(is.na(county))

# the entries with missing counties don't seem to be entries for water rights users, so remove these observations
wateruse_info2 = wateruse_info1 |> 
  filter(!is.na(county))

# look at observations with no monthly data to understand those that still have total use values
monthly_zeros = wateruse2 |>
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
wateruse3 |> 
  select(source_type) |> 
  unique()

wateruse3 |> 
  select(diversion_type) |> 
  unique()

wateruse3 |> 
  select(use_type) |> 
  unique()

# look at unique values of certain character variables for wateruse_info
wateruse_info2 |> 
  select(system_type) |> 
  unique()

wateruse_info2 |> 
  select(county) |> 
  unique()

# Relevant character variables look as they should, no need for logical checks

#------------------------------------------------------------------------------#
# Step 15: Clean string variables ----

# see if some use type categories can be combined
wateruse3 |> 
  select(use_type) |> 
  unique()

# combine power use types into one "Power" category
wateruse4 = wateruse3 |> 
  mutate(
    use_type = ifelse(
      use_type %in% c("Power (Hydro-Elec)", 
                      "Power (Fossil-Fuel)", 
                      "Power (Geothermal)",
                      "Geothermal"), 
      "Power", use_type))

#------------------------------------------------------------------------------#
# Additional cleaning ----

# aggregate yearly water usage by use type
wateruse_yearly = wateruse4 |> 
  select(year, use_type, total) |> 
  group_by(year, use_type) |> 
  summarize(total_use = sum(total))

# plot yearly water usage by use type
ggplot(wateruse_yearly, aes(x = year, y = log(total_use), color = use_type)) +
  geom_line() +
  facet_wrap(~use_type) +
  theme_minimal()

# filter out "Sewage Treatment" use type (very few years of data)
wateruse5 = wateruse4 |> 
  filter(use_type != "Sewage Treatment")
#------------------------------------------------------------------------------#

# create dataset copies for merging
wateruse_merge = wateruse5
wateruse_info_merge = wateruse_info2

# merge waterUse and waterUse_info datasets
wateruse_merged1 = left_join(wateruse_merge, wateruse_info_merge, 
                             by = "system_id", relationship = "many-to-many")

# drop duplicates, reorder columns, drop source_name
wateruse_merged2 = wateruse_merged1 |> 
  distinct() |>
  select(-source_name) |> 
  relocate(source_id, .after = system_id) |> 
  relocate(year, .after = source_id) |>
  relocate(county, .after = year) |>
  relocate(latitude, .after = county) |> 
  relocate(longitude, .after = latitude) |> 
  relocate(system_type, .after = source_type) |> 
  rename(total_gallons = total)


monthly_zeros = wateruse_merged2 |>
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

# convert to long format
wateruse_tidy1 = wateruse_merged2 |> 
  pivot_longer(cols = Jan:Dec,
               names_to = "month",
               values_to = "gallons")

#------------------------------------------------------------------------------#

# convert from population in thousands to actual population, rename, reorder variables, drop source_id (not relevant)
wateruse_tidy2 = wateruse_tidy1 |> 
  select(
    system_id,
    system_name,
    year,
    county,
    latitude,
    longitude,
    source_type,
    system_type,
    diversion_type,
    use_type,
    month,
    month_gallons = gallons,
    year_gallons = total_gallons
  )

#------------------------------------------------------------------------------#

# drop observations that have no monthly or yearly water use data
wateruse_tidy3 = wateruse_tidy2 |> 
  filter(year_gallons != 0)

# looking at the data, there are two observations with much higher usage than the rest of the data,
# and the water use data base is not consistent with these records, so these may have been an error caused during import
# Thus, we'll be removing these two observations below

# define surrogate key to remove the 2 observations
wateruse_tidy3 = wateruse_tidy3 |> 
  mutate(key = row_number())

# remove the two invalid observations
wateruse_tidy4 = wateruse_tidy3 |> 
  filter(key != 332034 & key != 332035)

#------------------------------------------------------------------------------#
# Potential cleaning ----

# calculate what months use the most water as a percentage of the total
totals = wateruse_tidy4 |>
  group_by(month) |> 
  summarize(monthly_total = sum(month_gallons, na.rm = TRUE)) |> 
  mutate(total = sum(monthly_total),
         month_percent = monthly_total / total)

# extract monthly percentages from totals dataframe
jan = totals[totals$month == "Jan", ]
jan_percent = jan$month_percent

feb = totals[totals$month == "Feb", ]
feb_percent = feb$month_percent

mar = totals[totals$month == "Mar", ]
mar_percent = mar$month_percent

apr = totals[totals$month == "Apr", ]
apr_percent = apr$month_percent

may = totals[totals$month == "May", ]
may_percent = may$month_percent

jun = totals[totals$month == "Jun", ]
jun_percent = jun$month_percent

jul = totals[totals$month == "Jul", ]
jul_percent = jul$month_percent

aug = totals[totals$month == "Aug", ]
aug_percent = aug$month_percent

sep = totals[totals$month == "Sep", ]
sep_percent = sep$month_percent

oct = totals[totals$month == "Oct", ]
oct_percent = oct$month_percent

nov = totals[totals$month == "Nov", ]
nov_percent = nov$month_percent

dec = totals[totals$month == "Dec", ]
dec_percent = dec$month_percent

total = totals$total
total = total[1]

# impute monthly averages for observations missing all months
wateruse_impute = wateruse_tidy4 |> 
  group_by(year_gallons) |> 
  mutate(month_gallons = ifelse(all(month_gallons == 0) & month == "Jan", jan_percent*year_gallons,
         ifelse(all(month_gallons == 0) & month == "Feb", feb_percent*year_gallons,
         ifelse(all(month_gallons == 0) & month == "Mar", mar_percent*year_gallons,
         ifelse(all(month_gallons == 0) & month == "Apr", apr_percent*year_gallons,
         ifelse(all(month_gallons == 0) & month == "May", may_percent*year_gallons,
         ifelse(all(month_gallons == 0) & month == "Jun", jun_percent*year_gallons,
         ifelse(all(month_gallons == 0) & month == "Jul", jul_percent*year_gallons,
         ifelse(all(month_gallons == 0) & month == "Aug", aug_percent*year_gallons,
         ifelse(all(month_gallons == 0) & month == "Sep", sep_percent*year_gallons,
         ifelse(all(month_gallons == 0) & month == "Oct", oct_percent*year_gallons,
         ifelse(all(month_gallons == 0) & month == "Nov", nov_percent*year_gallons,
         ifelse(all(month_gallons == 0) & month == "Dec", dec_percent*year_gallons, month_gallons)))))))))))))

# look at observations with no monthly data to check a random one (system_id == 1008 is what we chose)
check = wateruse_tidy4 |> 
  group_by(year_gallons) |> 
  filter(all(month_gallons == 0))

# check system_id = 1008 before imputing
sys1008_before = wateruse_tidy4 |> 
  filter(system_id == 1008 & year == 1988)

# check system_id = 1008 after imputing
sys1008_after = wateruse_impute |> 
  filter(system_id == 1008 & year == 1988)

## IT WORKED, WE ARE BETTER THAN EVERY DATA SCIENTIST EVER ##
#------------------------------------------------------------------------------#

wateruse_clean = wateruse_impute

# save as .rds file
save(wateruse_clean, file = "wateruse_clean.rds")
