library(dplyr)
library(tidyverse)
library(ggplot2)
library(data.table)
library(binsreg)

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
masterData = masterData |> 
  mutate(use_type = ifelse(use_type == "Irrigation", "Agricultural",
                    ifelse(use_type %in% c("Power (Hydro-Elec)", 
                                           "Power (Fossil-Fuel)", 
                                           "Power (Geothermal)",
                                           "Geothermal"),
                           "Power", use_type)))

# check to see if regrouping categories worked
unique_use_type2 = unique(masterData$use_type)
unique_use_type2

# look at Cargill Salt Inc.'s 2015 water use 
cargill = masterData |> 
  filter(system_id == 2168 & year == 2015 & source_type == "Lake") 

# remove Cargill Salt Inc. invalid observation for 2015 
masterData = as.data.table(masterData)
masterData = masterData |> 
  mutate(key = 1:nrow(masterData))

# remove invalid Cargill observations from master dataset
masterData = masterData |>
  filter(!(key %in% 331530:331539)) |> 
  select(-key)

#------------------------------------------------------------------------------#

# calculate total usage per use type category
use_type_total = masterData |> 
  group_by(use_type) |> 
  summarize(use_total = sum(year_gallons)) |> 
  arrange(desc(use_total))

# create a new data table with yearly aggregates 
yearly_data = masterData |> 
  group_by(year, use_type) |> 
  summarize(total_per_use = sum(year_gallons),
            gsl_level = mean(gsl_level, na.rm = TRUE),
            population = mean(population, na.rm = TRUE)) |>
  mutate(ln_total_per_use = log(total_per_use),
         usage_per_capita = total_per_use/population,
         ln_usage_per_capita = log(usage_per_capita)) |> 
  # remove sewage treatment because only data available for a few years and small water user
  filter(year >= 1970 & use_type != "Sewage Treatment") 

# calculate the total water usage per year
yearly_data = yearly_data |> 
  group_by(year) |> 
  mutate(total = sum(total_per_use)) |> 
  ungroup()

# create a graph showing water usage by type over time 
ggplot(yearly_data, aes(x = year, y = ln_total_per_use, color = reorder(use_type, -ln_total_per_use))) +
  geom_line() + 
  labs(
    title = "Log Yearly Water Usage by Use Type",
    x = "Year",  
    y = "Log Yearly Water Usage", 
    color = "Use Type"
  ) +
  scale_color_manual(values = c("Water Supplier" = "pink", 
                                "Agricultural" = "orange", 
                                "Industrial" = "cyan", 
                                "Mining" = "purple",
                                "Power" = "blue",
                                "Domestic" = "green",
                                "Commercial" = "red"
  )) +
  theme_minimal()

# create a graph showing GSL levels over time 
ggplot(yearly_data, aes(x = year, y = gsl_level)) + 
  geom_line(color = "blue") + 
  labs( 
    title = "Great Salt Lake Levels Over Time",
    x = "Year", 
    y = "Level in Feet") + 
  theme_minimal()

# create a graph showing water usage per capita 
ggplot(yearly_data, aes(x = year, y = ln_usage_per_capita)) +
  geom_smooth() + 
  labs( 
    title = "Water Usage Per Capita Over Time",
    x = "Year", 
    y = "Water Usage Per Capita") + 
  theme_minimal()

# create a new data set with yearly precipitation data for each county
precipitation = masterData |> 
  group_by(year, county) |> 
  summarize(month_precip = mean(county_precip),
            total_usage = sum(year_gallons))

# calculate yearly precipitation data for all counties
precipitation = precipitation |> 
  group_by(year) |> 
  summarize(precip = sum(month_precip),
            total_usage = sum(total_usage))

# create a scatterplot showing precipitation and water usage 
ggplot(precipitation, aes(x = precip, y = log(total_usage))) + 
  geom_point() + 
  geom_smooth(method = "lm") + 
  labs( 
    title = "Yearly Total Precipitation vs. Log Yearly Water Usage",
    x = "Total Precipitation in Inches", 
    y = "Log Yearly Water Usage") + 
  theme_minimal()



# normalize data to create different scales
gsl_max = max(yearly_data$gsl_level)
usage_max = max(yearly_data$ln_total)
gsl_min = min(yearly_data$gsl_level)

yearly_data$normalized_gsl <- (yearly_data$gsl_level - gsl_min) / (gsl_max - gsl_min)
yearly_data$normalized_usage <- yearly_data$ln_total / usage_max

# create a graph showing water use totals relative to GSL water levels 
ggplot() +
  geom_smooth(data = yearly_data, aes(x = year, y = normalized_gsl, color = "GSL Level")) +
  geom_smooth(data = yearly_data, aes(x = year, y = normalized_usage, color = "Log Yearly Water Use Total")) +
  geom_smooth(method = "lm", se = FALSE) +
  scale_y_continuous(
    sec.axis = sec_axis(~ . * usage_max, name = "Log Yearly Water Use Total"),
    name = "GSL Level"
  ) +
  labs(
    title = "GSL Water Levels and Water Use Totals Over Time",
    x = "Year"
  ) +
  theme_minimal() +
  theme(legend.position = "bottomright") +
  scale_color_manual(values = c("GSL Level" = "green", "Log Yearly Water Use Total" = "red"))







