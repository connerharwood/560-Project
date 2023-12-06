library(dplyr)
library(tidyverse)
library(skimr)
library(data.table)
library(sf)

load("~/560-Project/raw-data/data/wateruse_raw.rda")

#------------------------------------------------------------------------------#
# Step 3: Remove irrelevant, garbage, or empty rows and columns ----

# select & rename relevant variables from wateruse_raw
wateruse1 = wateruse_data |> 
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
wateruse3 = wateruse2 |> 
  filter(!is.na(total))

# remove observations with NA use type
wateruse3 = wateruse3 |> 
  filter(!is.na(use_type))

# explore data again
skim(wateruse3)

# convert invalid latitude and longitude to NA, then remove them
wateruse3 = wateruse3 |> 
  mutate(latitude = ifelse(latitude < 37 | longitude == 0, NA, latitude),
         longitude = ifelse(latitude < 37 | longitude == 0, NA, longitude)) |> 
  filter(!is.na(latitude) | !is.na(longitude))

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

wateruse4 |> 
  select(use_type) |> 
  unique()

#------------------------------------------------------------------------------#
# Merge ----

# create dataset copies for merging
wateruse_merge = wateruse4
wateruse_info_merge = wateruse_info2

# merge wateruse and wateruse_info datasets
wateruse_merged = left_join(wateruse_merge, wateruse_info_merge, 
                            by = "system_id", relationship = "many-to-many")

#------------------------------------------------------------------------------#
# Spatial ----

# we found a shapefile for the boundaries of the Great Salt Lake Basin
# so going to filter out all water use observations that don't lie within this basin

# Great Salt Lake Basin shapefile
gsl_basin = st_read("~/560-Project/spatial/data/basin/GSLSubbasins.shp")

# filter out Strawberry Reservoir, drop duplicate column
gsl_basin = gsl_basin |> 
  filter(Name != "Strawberry") |> 
  select(-Shape_Le_1)

# convert GSL Basin shapefile to WGS 84 CRS
gsl_basin = st_transform(gsl_basin, crs = st_crs("+proj=longlat +datum=WGS84"))

# convert water use data to sf object
wateruse_sf = st_as_sf(wateruse_merged, coords = c("longitude", "latitude"), crs = st_crs(gsl_basin))

# simplify geometry to run st_intersection faster
gsl_basin = st_simplify(gsl_basin, preserveTopology = TRUE)
wateruse_sf = st_simplify(wateruse_sf, preserveTopology = TRUE)

# include only water use observations that lie within the GSL basin
intersections = st_intersection(wateruse_sf, gsl_basin)

# extract latitude and longitude from geometry column
wateruse_within = st_as_sf(intersections, wkt = "geometry")
wateruse_within$longitude = st_coordinates(wateruse_within)[, "X"]
wateruse_within$latitude = st_coordinates(wateruse_within)[, "Y"]

wateruse5 = as.data.table(wateruse_within)

# drop irrelevant variables
wateruse6 = wateruse5 |> 
  select(-ID, -GRIDCODE, -Shape_Leng, -Shape_Area, -geometry, subbasin = Name)

# check if correct coordinates were included
max(wateruse6$latitude) # 41.99898
min(wateruse6$latitude) # 39.6587

max(wateruse6$longitude) # -110.7936
min(wateruse6$longitude) # -113.1804

max(st_coordinates(gsl_basin)[, "X"]) # -110.6139
min(st_coordinates(gsl_basin)[, "X"]) # -113.3983

max(st_coordinates(gsl_basin)[, "Y"]) # 42.83881
min(st_coordinates(gsl_basin)[, "Y"]) # 39.58265

# remove duplicates
wateruse7 = wateruse6 |> 
  distinct()

# see what counties were included
wateruse6 |> 
  distinct(county)
# based on this, we added 3 counties (Summit, Wasatch, and Juab to our precipitation and population data)

# filter out counties that don't lie in the basin (incorrect coordinates)
wateruse8 = wateruse7 |>
  filter(!(county %in% c("Duchesne", 
                         "Iron",
                         "Emery", 
                         "Wayne", 
                         "Sanpete", 
                         "Washington", 
                         "Kane", 
                         "Uintah", 
                         "San Juan", 
                         "Daggett", 
                         "Grand")))

save(wateruse8, file = "wateruse_within_backup.rds")

#------------------------------------------------------------------------------#
# Balance the panel data ----

# After talking with Nick and thinking more about the inconsistencies in reporting, we looked deeper into Utah water reporting
# laws, as well as the number of water rights per sector being issued each year
# From this, we found a drastic increase in water rights users in our data, and found that this is likely due to a 2015
# law changing water use reporting requirements
# Thus, we filtered out observations before 2015, and looked at changes in number of users during this period (1959-2014)
# During this period, we looked at trends in number of water users per year for each use type and saw a consistent upward
# trend among all use types except mining and sewage treatment between 1996 and 2014
# Thus, we filtered out mining and sewage treatment use types to have consistent reporting across use types and restricted
# our data to the period 1996-2014

# sum number of water users reporting per year for each use type
users_per_year = wateruse8 |> 
  group_by(year, use_type) |> 
  summarize(n_system_id = n_distinct(system_id))

# facet scatterplot of number of users reporting per year for each use type
user_report_plot = ggplot(users_per_year, aes(x = year, y = n_system_id, color = use_type)) +
  geom_point(size = 0.5) +
  facet_wrap(~use_type, scale = "free_y") + 
  labs(
    title = "System Users by Use Type", 
    x = "Year", 
    y = "Count"
  ) +
  scale_color_manual(
    values = c(
      "Agricultural" = "black",
      "Irrigation" = "#E69F00",
      "Water Supplier" = "#56B4E9",
      "Industrial" = "#009E73",
      "Power" = "#CC79A7",
      "Domestic" = "#0072B2",
      "Commercial" = "#D55E00"
    )) + 
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 15), 
        legend.position = "none")

print(user_report_plot)

# save as higher resolution png image
ggsave(
  filename = "user_report_plot.png",
  plot = user_report_plot,
  height = 6,
  width = 8.5,
  units = "in",
  dpi = 300,
)

# restrict data to 1996-2014 when reporting was consistent
users_1996_2014 = wateruse8 |> 
  filter(year >= 1996 & year <= 2014) |> 
  group_by(year, use_type) |> 
  summarize(n_system_id = n_distinct(system_id))

# facet scatterplot of number of users reporting per year for each use type, 1996-2014
user_1996_2014_plot = ggplot(users_1996_2014, aes(x = year, y = n_system_id, color = use_type)) +
  geom_point(size = 0.5) +
  facet_wrap(~use_type, scale = "free_y") +
  labs(
    title = "System Users by Use Type (1996-2014)", 
    x = "Year", 
    y = "Count"
  ) +
  scale_color_manual(
    values = c(
      "Agricultural" = "black",
      "Irrigation" = "#E69F00",
      "Water Supplier" = "#56B4E9",
      "Industrial" = "#009E73",
      "Power" = "#CC79A7",
      "Domestic" = "#0072B2",
      "Commercial" = "#D55E00"
    )) + 
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 15),
        legend.position = "none") +
  scale_x_continuous(breaks = seq(1996, 2014, by = 6))

print(user_1996_2014_plot)

# save as higher resolution png image
ggsave(
  filename = "user_1996_2014_plot.png",
  plot = user_1996_2014_plot,
  height = 6,
  width = 8.5,
  units = "in",
  dpi = 300,
)

# filter dataset to period 1996-2014, filter out mining
wateruse9 = wateruse8 |> 
  filter(year >= 1996 & year <= 2014 & use_type != "Mining")

#------------------------------------------------------------------------------#
# Additional cleaning ----

# add surrogate key, reorder columns
wateruse10 = wateruse9 |>
  mutate(
    key = row_number()
  ) |>
  select(
    key,
    system_id,
    system_name,
    source_id,
    source_name,
    year,
    county,
    subbasin,
    use_type,
    diversion_type,
    system_type,
    source_type,
    Jan:Dec,
    total,
    latitude,
    longitude
  )

# look at observations with total of 0
total_zero = wateruse10 |> 
  filter(total == 0)

skim(total_zero)

# observations with 0 reported for total also report no monthly data, so filter them out
wateruse11 = wateruse10 |> 
  filter(total != 0)

# look at observations where all months report 0
monthly_zeros = wateruse11 |>
  group_by(system_id, source_id) |> 
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

# for now keep observations reporting a total but nothing for all months for use with yearly data
# might filter out in the future if we analyze monthly data

# convert to long format
wateruse12 = wateruse11 |> 
  pivot_longer(cols = Jan:Dec,
               names_to = "month",
               values_to = "gallons")

# reorder and rename
wateruse13 = wateruse12 |> 
  rename(month_gallons = gallons, year_gallons = total) |> 
  relocate(month, .after = year) |> 
  relocate(month_gallons, .after = source_type)

#------------------------------------------------------------------------------#
# Extreme values ----

# yearly total water usage per use type to look at extreme values
wateruse_yearly = wateruse13 |> 
  select(year, use_type, year_gallons) |> 
  group_by(year, use_type) |> 
  summarize(
    year_gallons = sum(year_gallons)
  )

# plot yearly water use by use type to look at extreme values
ggplot(wateruse_yearly, aes(x = year, y = year_gallons, color = use_type)) +
  geom_line() +
  facet_wrap(~use_type, scale = "free_y")

# looking at the yearly usage plot, there are some big spikes in the "Water Supplier" use type

# "Water Supplier" usage spike around 2006
watersupplier2005_2007 = wateruse13 |> 
  filter(use_type == "Water Supplier") |> 
  filter(year == 2005 | year == 2006 | year == 2007) |> 
  arrange(desc(year_gallons))

# "Water Supplier" usage spike around 2014
watersupplier2013_2015 = wateruse13 |> 
  filter(use_type == "Water Supplier") |> 
  filter(year == 2013 | year == 2014 | year == 2015) |> 
  arrange(desc(year_gallons))

# select specific observations that are likely invalid
watersupplier2006 = watersupplier2005_2007 |> 
  mutate(n = row_number()) |> 
  filter(n <= 24) |> 
  pull(key)

# select specific observations that are likely invalid
watersupplier2014 = watersupplier2013_2015 |> 
  mutate(n = row_number()) |> 
  filter(n <= 60) |> 
  pull(key)

# observations to remove
watersupplier_remove = c(watersupplier2006, watersupplier2014)

# remove 2006 and 2014 "Water Supplier" outliers
wateruse14 = wateruse13 |> 
  filter(!(key %in% watersupplier_remove))

# yearly total water usage per use type to look at extreme values
wateruse_yearly = wateruse14 |> 
  select(year, use_type, year_gallons) |> 
  group_by(year, use_type) |> 
  summarize(
    year_gallons = sum(year_gallons)
  )

# plot yearly water use by use type to see if filter took care of extreme values
ggplot(wateruse_yearly, aes(x = year, y = year_gallons, color = use_type)) +
  geom_line() +
  facet_wrap(~use_type, scale = "free_y")

# convert month variable to uppercase for merging with other datasets later
wateruse15 = wateruse14 |> 
  mutate(month = str_to_title(month))

#------------------------------------------------------------------------------#
# Save ----

wateruse_clean = wateruse15

save(wateruse_clean, file = "wateruse_clean.rds")

#------------------------------------------------------------------------------#
# 1996-2022 ----

# for robustness, we'll also run a regression with 1996-2022 data

# filter dataset to period 1996-2022, filter out mining and sewage treatment
wateruse_all1 = wateruse8 |> 
  filter(year >= 1996 & use_type != "Mining" & use_type != "Sewage")

# add surrogate key, reorder columns
wateruse_all2 = wateruse_all1 |>
  mutate(
    key = row_number()
  ) |>
  select(
    key,
    system_id,
    system_name,
    source_id,
    source_name,
    year,
    county,
    subbasin,
    use_type,
    diversion_type,
    system_type,
    source_type,
    Jan:Dec,
    total,
    latitude,
    longitude
  )

# observations with 0 reported for total also report no monthly data, so filter them out
wateruse_all3 = wateruse_all2 |> 
  filter(total != 0)

# convert to long format
wateruse_all4 = wateruse_all3 |> 
  pivot_longer(cols = Jan:Dec,
               names_to = "month",
               values_to = "gallons")

# reorder and rename
wateruse_all5 = wateruse_all4 |> 
  rename(month_gallons = gallons, year_gallons = total) |> 
  relocate(month, .after = year) |> 
  relocate(month_gallons, .after = source_type)

#------------------------------------------------------------------------------#
# Extreme values 1996-2022 ----

# yearly total water usage per use type to look at extreme values
wateruse_yearly_all = wateruse_all5 |> 
  select(year, use_type, year_gallons) |> 
  group_by(year, use_type) |> 
  summarize(
    year_gallons = sum(year_gallons)
  )

# plot yearly water use by use type to look at extreme values
ggplot(wateruse_yearly_all, aes(x = year, y = year_gallons, color = use_type)) +
  geom_line() +
  facet_wrap(~use_type, scale = "free_y")

# looking at the yearly usage plot, there are some big spikes in the "Industrial" use type
# we previously looked at "Water Supplier" extreme values, which will be removed as well

# looking at the yearly usage plot, there are some big spikes in the "Water Supplier" use type

# "Water Supplier" usage spike in 2006
watersupplier_all2006 = wateruse_all5 |> 
  filter(use_type == "Water Supplier") |> 
  filter(year == 2006) |> 
  arrange(desc(year_gallons))

# "Water Supplier" usage spike in 2014
watersupplier_all2014 = wateruse_all5 |> 
  filter(use_type == "Water Supplier") |> 
  filter(year == 2014) |> 
  arrange(desc(year_gallons))

# select specific observations that are likely invalid
watersupplier_remove2006 = watersupplier_all2006 |> 
  mutate(n = row_number()) |> 
  filter(n <= 24) |> 
  pull(key)

# select specific observations that are likely invalid
watersupplier_remove2014 = watersupplier_all2014 |> 
  mutate(n = row_number()) |> 
  filter(n <= 72) |> 
  pull(key)

# observations to remove
watersupplier_remove_all = c(watersupplier_remove2006, watersupplier_remove2014)

# "Industrial" usage spike in 2015
industrial_all2015 = wateruse_all5 |> 
  filter(use_type == "Industrial") |> 
  filter(year == 2015) |> 
  arrange(desc(year_gallons))

# select specific observations that are likely invalid
industrial_remove = industrial_all2015 |> 
  mutate(n = row_number()) |> 
  filter(n <= 12) |> 
  pull(key)

# remove 2006 and 2014 "Water Supplier" outliers and 2015 "Industrial " outliers
wateruse_all6 = wateruse_all5 |> 
  filter(
    !(key %in% watersupplier_remove_all) ,
    !(key %in% industrial_remove)
  )

# yearly total water usage per use type to look at extreme values again
wateruse_yearly_all2 = wateruse_all6 |> 
  select(year, use_type, year_gallons) |> 
  group_by(year, use_type) |> 
  summarize(
    year_gallons = sum(year_gallons)
  )

# plot yearly water use by use type to look at extreme values again
ggplot(wateruse_yearly_all2, aes(x = year, y = year_gallons, color = use_type)) +
  geom_line() +
  facet_wrap(~use_type, scale = "free_y")

# convert month variable to uppercase for merging with other datasets later
wateruse_all7 = wateruse_all6 |> 
  mutate(month = str_to_title(month))

wateruse_1996_2022 = wateruse_all7

#------------------------------------------------------------------------------#
# Save 1996-2022 ----

save(wateruse_1996_2022, file = "wateruse_1996_2022.rds")
