library(dplyr)
library(tidyverse)
library(ggplot2)
library(data.table)
library(binsreg)
library(scales)
library(lubridate)

load("~/560-Project/clean-data/data/masterData.rds")

#------------------------------------------------------------------------------#

# create a graph showing water usage by type over time 
plot1 = ggplot(masterData, aes(x = year, y = log(total_gallons), color = reorder(water_use, -total_gallons))) +
  geom_smooth(se = FALSE, span = 0.09, size = 0.5) +
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
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# save plot as .png file
ggsave("waterUse_byType.png", plot = plot1)

# create a graph showing GSL levels over time 
plot2 = ggplot(masterData, aes(x = year, y = gsl_level)) + 
  geom_line(color = "blue") +
  geom_hline(yintercept = 4198, linetype = "dashed", color = "red") +
  geom_text(aes(x = max(year), y = 4198.5, label = "Minimum Healthy Lake Level", color = "red"), hjust = 1) +
  labs( 
    title = "Great Salt Lake Levels",
    x = "Year", 
    y = "Level in Feet") + 
  theme_minimal() +
  guides(color = "none") +
  theme(plot.title = element_text(hjust = 0.5))

# save plot as .png file
ggsave("gslLevels.png", plot = plot2)

# create a graph showing water usage per capita 
plot3 = ggplot(masterData, aes(x = year, y = perCapita_usage / 1000)) +
  geom_smooth(span = 0.01) +
  labs( 
    title = "Water Usage Per Capita Over Time",
    x = "Year", 
    y = "Water Usage Per Capita") + 
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# save plot as .png file
ggsave("")

# create a scatterplot showing precipitation and water usage 
ggplot(masterData, aes(x = precipitation, y = log(total_gallons), color = water_use)) + 
  geom_smooth(span = 0.5, se = FALSE) + 
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

#------------------------------------------------------------------------------#
## Create separate dataset to focus on water-related land use data ##

# select only agricultural water use and water-related land use data
agData1 = masterData |> 
  filter(water_use == "Agricultural" | land_use == c("IR", "AG", "SUBIR"))

# aggregate total acreage per year across agricultural land uses
agData2 = agData1 |> 
  distinct(year, land_use, total_acres, .keep_all = TRUE) |> 
  group_by(year) |> 
  mutate(yearly_acres = sum(total_acres))

# aggregate total water usage per year across agricultural water uses
agData = agData2 |> 
  distinct(year, water_use, total_gallons, .keep_all = TRUE) |> 
  group_by(year) |> 
  mutate(yearly_usage = sum(total_gallons))

# graph yearly water usage and water-related land usage per year in the agricultural sector
ggplot() +
  geom_smooth(data = agData, aes(x = year, y = yearly_acres, color = "Ag Land Use"), se = FALSE) +
  geom_smooth(data = agData, aes(x = year, y = yearly_usage / 1000000, color = "Ag Water Usage"), se = FALSE) +
  scale_y_continuous(
    name = "Acres",
    sec.axis = sec_axis(~./10, name = "Gallons in Millions")
  ) +
  labs(
    x = "Year",
    title = "Agricultural Water Use and Water-Related Land Use"
  ) +
  theme_minimal()
