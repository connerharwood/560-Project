library(dplyr)
library(tidyverse)
library(ggplot2)
library(data.table)
library(binsreg)

load("~/560-Project/clean-data/data/masterData.rds")

#------------------------------------------------------------------------------#

## preparation and some additional cleaning to the master data ##

masterData = as.data.table(masterData)

# remove duplicates in the master dataset
masterData = masterData |> 
  distinct()

# check structure of master dataset
str(masterData)

# look at use type categories
unique_use_type = unique(masterData$water_use)
unique_use_type

# group similar use type categories
masterData = masterData |> 
  mutate(water_use = ifelse(water_use == "Irrigation", "Agricultural",
                    ifelse(water_use %in% c("Power (Hydro-Elec)", 
                                           "Power (Fossil-Fuel)", 
                                           "Power (Geothermal)",
                                           "Geothermal"),
                           "Power", water_use))) |> 
  # remove sewage since only has a few years of observation and small water user
  filter(water_use != "Sewage Treatment" & year >= 1970) 

# check to see if regrouping categories worked
unique_use_type2 = unique(masterData$water_use)
unique_use_type2

#------------------------------------------------------------------------------#

# create a graph showing water usage by type over time 
ggplot(masterData, aes(x = year, y = log(total_use), color = reorder(use_type, -total_use))) +
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
ggplot(masterData, aes(x = year, y = gsl_level)) + 
  geom_line(color = "blue") + 
  geom_hline(yintercept = 4198, linetype = "dashed", color = "red") +
  labs( 
    title = "Great Salt Lake Levels",
    x = "Year", 
    y = "Level in Feet") + 
  theme_minimal()
# idea for GSL levels graph: plot line at 4198 feet, showing minimum level needed for a healthy lake !!!!!!!

# create a graph showing water usage per capita 
ggplot(masterData, aes(x = year, y = ln_usage_per_capita)) +
  geom_smooth() + 
  labs( 
    title = "Water Usage Per Capita Over Time",
    x = "Year", 
    y = "Water Usage Per Capita") + 
  theme_minimal()

# create a scatterplot showing precipitation and water usage 
ggplot(masterData, aes(x = precipitation, y = log(total_gallons))) + 
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







