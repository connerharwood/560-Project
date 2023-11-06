library(dplyr)
library(tidyverse)
library(ggplot2)
library(data.table)
library(binsreg)

load("~/560-Project/clean-data/data/masterData.rds")

#------------------------------------------------------------------------------#

# create a graph showing water usage by type over time 
ggplot(masterData, aes(x = year, y = log(total_gallons), color = reorder(water_use, -total_gallons))) +
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
