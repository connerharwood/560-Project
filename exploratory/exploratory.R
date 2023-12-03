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
useType_totals = masterdata |> 
  group_by(water_use) |> 
  summarize("Total Gallons in Hundreds of Billions" = sum(total_gallons) / 100000000000) |> 
  rename("Water Use Type" = water_use) |> 
  arrange(desc(`Total Gallons in Hundreds of Billions`))

# save data table as .rds file to put in R Markdown
save(useType_totals, file = "useType_totals.rds")

# create a graph showing water usage by type over time 
plot1 = ggplot(masterData, aes(x = year, y = log(total_gallons), color = reorder(water_use, -total_gallons))) +
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
    title = "Water Usage Per Capita",
    x = "Year", 
    y = "Water Usage Per Capita") + 
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# save plot as .png file
ggsave("perCapita_usage.png", plot = plot3)

# create a scatterplot showing precipitation and water usage 
plot4 = ggplot(masterData, aes(x = precipitation, y = log(total_gallons), color = water_use)) + 
  geom_smooth(span = 0.5, se = FALSE) + 
  labs( 
    title = "Yearly Total Precipitation vs. Log Yearly Water Usage",
    x = "Total Precipitation in Inches", 
    y = "Log Yearly Water Usage") + 
  theme_minimal()

# save plot as .png file
ggsave("precip_waterUsage.png", plot = plot4)

#------------------------------------------------------------------------------#
## Create separate dataset to focus on agricultural water-related land use and water use data ##

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
plot5 = ggplot() +
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

# save plot as .png file
ggsave("agPlots.png", plot = plot5)

#------------------------------------------------------------------------------#
## Create separate dataset to focus on agricultural water use data and GSL levels ##

# select only agricultural water use and water-related land use data
agWater_GSL1 = masterData |> 
  filter(water_use == "Agricultural")

# aggregate total water usage per year across agricultural water uses
agWater_GSL = agWater_GSL1 |> 
  distinct(year, water_use, total_gallons, .keep_all = TRUE) |> 
  group_by(year) |> 
  mutate(yearly_usage = sum(total_gallons))

# create a plot showing GSL levels in feet over time
plot_gsl_levels = ggplot(agWater_GSL, aes(x = year, y = gsl_level)) +
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
plot_ag_water_usage = ggplot(agWater_GSL, aes(x = year, y = log(yearly_usage))) +
  geom_smooth(color = "pink", se = FALSE, span = 0.1) +
  labs(y = "Log Ag Water Use", 
       x = "Year") +
  theme_minimal()

# combine the two plots into one 
plot6 = plot_grid(
  plot_gsl_levels,
  plot_ag_water_usage,
  ncol = 1,
  align = "v"
)

print(plot6)

# save plot as .png file
ggsave("agWater_GSL.png", plot = plot6)

