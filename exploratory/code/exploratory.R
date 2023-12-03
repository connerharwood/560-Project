library(dplyr)
library(tidyverse)
library(ggplot2)
library(data.table)
library(binsreg)
library(scales)
library(lubridate)
library(cowplot)

load("~/560-Project/clean-data/data/masterdata.rds")

#------------------------------------------------------------------------------#

# create table showing total water use by use type
use_type_totals = masterdata |> 
  group_by(use_type) |> 
  summarize("Total Gallons in Hundreds of Billions" = sum(year_gallons) / 100000000000) |> 
  rename("Water Use Type" = use_type) |> 
  arrange(desc(`Total Gallons in Hundreds of Billions`))

# create a graph showing water usage by use type over time 
plot1 = ggplot(masterdata, aes(x = year, y = log(year_gallons), color = reorder(use_type, -year_gallons))) +
  geom_smooth(se = FALSE, span = 0.09, size = 0.5) +
  labs(
    title = "Log Yearly Water Usage by Use Type",
    x = "Year",  
    y = "Log Yearly Water Usage", 
    color = "Use Type"
  ) +
  scale_color_brewer(palette = "Set2") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

print(plot1)

# save plot as .png file
ggsave("wateruse_by_type.png", plot = plot1)

# create a graph showing GSL levels over time 
plot2 = ggplot(masterdata, aes(x = year, y = gsl_level_ft)) + 
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

print(plot2)

# save plot as .png file
ggsave("gsl_levels.png", plot = plot2)

# create a graph showing water usage per capita 
plot3 = ggplot(masterdata, aes(x = year, y = percapita_usage / 1000)) +
  geom_smooth(span = 0.01) +
  labs( 
    title = "Water Usage Per Capita",
    x = "Year", 
    y = "Water Usage Per Capita") + 
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

print(plot3)

# save plot as .png file
ggsave("percapita_usage.png", plot = plot3)

# create a scatterplot showing precipitation and water usage 
plot4 = ggplot(masterdata, aes(x = precip_in, y = log(year_gallons), color = use_type)) + 
  geom_smooth(span = 0.5, se = FALSE) + 
  labs( 
    title = "Yearly Total Precipitation vs. Log Yearly Water Usage",
    x = "Total Precipitation in Inches", 
    y = "Log Yearly Water Usage") + 
  theme_minimal()

print(plot4)

# save plot as .png file
ggsave("precip_wateruse.png", plot = plot4)

#------------------------------------------------------------------------------#
## Create separate dataset to focus on agricultural water-related land use and water use data ##

# select only agricultural water use and water-related land use data
agdata1 = masterdata |> 
  filter(use_type == "Agricultural")

# aggregate total water usage per year across agricultural water uses
agdata = agdata1 |> 
  distinct(year, use_type, year_gallons, .keep_all = TRUE) |> 
  group_by(year) |> 
  mutate(total_gallons = sum(year_gallons))

# graph yearly water usage and water-related land usage per year in the agricultural sector
plot5 = ggplot() +
  geom_smooth(data = agdata, aes(x = year, y = yearly_acres, color = "Ag Land Use"), se = FALSE) +
  geom_smooth(data = agdata, aes(x = year, y = yearly_usage / 1000000, color = "Ag Water Usage"), se = FALSE) +
  scale_y_continuous(
    name = "Acres",
    sec.axis = sec_axis(~./10, name = "Gallons in Millions")
  ) +
  labs(
    x = "Year",
    title = "Agricultural Water Use and Water-Related Land Use"
  ) +
  theme_minimal()

# save plot as .png file
ggsave("agplots.png", plot = plot5)

#------------------------------------------------------------------------------#
## Create separate dataset to focus on agricultural water use data and GSL levels ##

# select only agricultural water use
ag_gsl1 = masterdata |> 
  filter(water_use == "Agricultural")

# aggregate total water usage per year across agricultural water uses
ag_gsl = ag_gsl1 |> 
  distinct(year, use_type, year_gallons, .keep_all = TRUE) |> 
  group_by(year) |> 
  mutate(total_gallons = sum(year_gallons))

# create a plot showing GSL levels in feet over time
plot_gsl_levels = ggplot(ag_gsl, aes(x = year, y = gsl_level_ft)) +
  geom_smooth(color = "lightblue", se = FALSE, span = 0.1) +
  labs(y = "GSL Levels (Feet)",
       title = "Agricultural Water Use and GSL Levels" 
       ) +
  geom_hline(yintercept = 4198, linetype = "dashed", color = "blue") +
  geom_text(aes(x = max(year), y = 4198.5, label = "Minimum Healthy Level = 4198", color = "blue"), hjust = 1, size = 3) +
  guides(color = "none") +
  theme_minimal() +
  # remove x-axis since irrelevant 
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.title.x = element_blank()
  ) +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_color_manual(values = c("blue" = "blue"))

# create a plot showing agricultural water usage 
plot_ag_water_use = ggplot(ag_gsl, aes(x = year, y = log(total_gallons))) +
  geom_smooth(color = "pink", se = FALSE, span = 0.1) +
  labs(y = "Log Ag Water Use", 
       x = "Year") +
  theme_minimal()

# combine the two plots into one 
plot6 = plot_grid(
  plot_gsl_levels,
  plot_ag_water_use,
  ncol = 1,
  align = "v"
)

print(plot6)

# save plot as .png file
ggsave("ag_gsl.png", plot = plot6)
