library(dplyr)
library(tidyverse)
library(zoo)
library(binsreg)
library(scales)
library(lubridate)
library(cowplot)

load("~/560-Project/clean-data/data/masterdata.rds")

# notes (delete when done)

# 5 plots that we've tried in this script:

# 1. water usage by use type (included in Stage 2 submission)

# 2. GSL levels plot
# we plotted GSL levels against ag use, so not necessary to focus on this plot

# 3. Ag water use vs GSL levels plot (included in Stage 2)
# need to do ag water use vs GSL volume to be on same scale (should GSL volume be log transformed since water use is?)

# 4. water use per capita plot (included in Stage 2)

# 5. precipitation vs water usage (included in Stage 2)

# also have a table of water usage by use type (total across all the years), not sure if we care to use this one at all
# the masterdata, yearly_per_use, and yearly_total_use don't have any transformed variables, so do that in ggplot if necessary
# log transform some, or maybe divide units with very large numbers by 10000 or so

# also could be worth looking into monthly level plots

#------------------------------------------------------------------------------#
# Aggregate yearly data ----

# GSL yearly average level and volume
gsl_yearly = masterdata |> 
  # convert volume from cubic meters to gallons
  mutate(gsl_volume_gal = gsl_volume_m3 * 264.172052) |> 
  select(year, gsl_level_ft, gsl_volume_gal) |> 
  group_by(year) |> 
  summarize(
    gsl_level_ft = mean(gsl_level_ft),
    gsl_volume_gal = mean(gsl_volume_gal)
  ) |> 
  arrange(year) |> 
  # calculate year-over-year change in level and volume
  mutate(
    gsl_level_change = (gsl_level_ft - lag(gsl_level_ft)) / lag(gsl_level_ft) * 100,
    gsl_volume_change = (gsl_volume_gal - lag(gsl_volume_gal)) / lag(gsl_volume_gal) * 100
  )

# yearly total population across GSL Basin counties
pop_yearly = masterdata |> 
  select(year, county, population_thousands) |> 
  mutate(population = population_thousands * 1000) |> 
  group_by(year, county) |> 
  summarize(
    population = mean(population)
  ) |> 
  group_by(year) |> 
  summarize(
    population = sum(population)
  )

# yearly average precipitation across GSL Basin counties
precip_yearly = masterdata |> 
  select(year, month, county, precip_in) |> 
  group_by(year, month, county) |> 
  summarize(
    precip_in = mean(precip_in)
  ) |> 
  group_by(year, county) |> 
  summarize(
    precip_in = sum(precip_in)
  ) |> 
  group_by(year) |> 
  summarize(
    precip_in = mean(precip_in)
  )

# yearly total water usage per use type
wateruse_yearly = masterdata |> 
  select(year, use_type, year_gallons) |> 
  group_by(year, use_type) |> 
  summarize(
    year_gallons = sum(year_gallons)
  )

# merge yearly datasets into one
yearly_merge1 = left_join(wateruse_yearly, gsl_yearly, by = "year", relationship = "many-to-one")
yearly_merge2 = left_join(yearly_merge1, pop_yearly, by = "year", relationship = "many-to-one")
yearly_merge3 = left_join(yearly_merge2, precip_yearly, by = "year", relationship = "many-to-one")

# yearly usage data for each use type
yearly_per_use = yearly_merge3 |> 
  # calculate yearly per capita water usage for each use type
  mutate(
    percapita_usage = year_gallons / population
  )

# yearly total usage data across all use types
yearly_total_use = yearly_merge3 |> 
  group_by(year) |> 
  summarize(
    year_gallons = sum(year_gallons),
    gsl_level_ft = mean(gsl_level_ft),
    gsl_volume_gal = mean(gsl_volume_gal),
    gsl_level_change = mean(gsl_level_change),
    gsl_volume_change = mean(gsl_volume_change),
    population = mean(population),
    precip_in = mean(precip_in)
  ) |> 
  # calculate yearly per capita water usage
  mutate(
    percapita_usage = year_gallons / population
  )

#------------------------------------------------------------------------------#
# Aggregate monthly data ----

# convert month and year to date format
masterdata_monthly = masterdata |> 
  mutate(
    date = as.Date(paste(year, month, "01"), format = "%Y %b %d"),
    date = as.yearmon(date)
  )

# GSL monthly level and volume
gsl_monthly = masterdata_monthly |> 
  # convert volume from cubic meters to gallons
  mutate(
    gsl_volume_gal = gsl_volume_m3 * 264.172052
  ) |> 
  select(date, gsl_level_ft, gsl_volume_gal) |> 
  group_by(date) |> 
  summarize(
    gsl_level_ft = mean(gsl_level_ft),
    gsl_volume_gal = mean(gsl_volume_gal)
  ) |> 
  arrange(date) |> 
  # calculate year-over-year change in level and volume
  mutate(
    gsl_level_change = (gsl_level_ft - lag(gsl_level_ft)) / lag(gsl_level_ft) * 100,
    gsl_volume_change = (gsl_volume_gal - lag(gsl_volume_gal)) / lag(gsl_volume_gal) * 100
  )

# monthly total population across GSL Basin counties
pop_monthly = masterdata_monthly |> 
  select(date, county, population_thousands) |> 
  mutate(population = population_thousands * 1000) |> 
  group_by(date, county) |> 
  summarize(
    population = mean(population)
  ) |> 
  group_by(date) |> 
  summarize(
    population = sum(population)
  )

# monthly average precipitation across GSL Basin counties
precip_monthly = masterdata_monthly |> 
  select(date, county, precip_in) |> 
  group_by(date, county) |> 
  summarize(
    precip_in = mean(precip_in)
  ) |> 
  group_by(date) |> 
  summarize(
    precip_in = mean(precip_in)
  )

# monthly total water usage per use type
wateruse_monthly = masterdata_monthly |>
  filter(month_gallons != 0) |> 
  select(date, use_type, month_gallons) |> 
  group_by(date, use_type) |> 
  summarize(
    month_gallons = sum(month_gallons)
  )

# merge monthly datasets into one
monthly_merge1 = left_join(wateruse_monthly, gsl_monthly, by = "date", relationship = "many-to-one")
monthly_merge2 = left_join(monthly_merge1, pop_monthly, by = "date", relationship = "many-to-one")
monthly_merge3 = left_join(monthly_merge2, precip_monthly, by = "date", relationship = "many-to-one")

# monthly usage data for each use type
monthly_per_use = monthly_merge3 |> 
  # calculate monthly per capita water usage for each use type
  mutate(
    percapita_usage = month_gallons / population
  )

# monthly total usage data across all use types
monthly_total_use = monthly_merge3 |> 
  group_by(date) |> 
  summarize(
    month_gallons = sum(month_gallons),
    gsl_level_ft = mean(gsl_level_ft),
    gsl_volume_gal = mean(gsl_volume_gal),
    gsl_level_change = mean(gsl_level_change),
    gsl_volume_change = mean(gsl_volume_change),
    population = mean(population),
    precip_in = mean(precip_in)
  ) |> 
  # calculate yearly per capita water usage
  mutate(
    percapita_usage = month_gallons / population
  )

#------------------------------------------------------------------------------#
# Water usage by use type plot ----

plot1 = ggplot() +
  geom_line(
    data = yearly_per_use[yearly_per_use$use_type != "Agricultural", ], 
    aes(x = year, y = log(year_gallons), color = use_type), 
    size = 0.5, 
    alpha = 0.55) +
  geom_line(
    data = yearly_per_use[yearly_per_use$use_type == "Agricultural", ], 
    aes(x = year, y = log(year_gallons), color = use_type), 
    size = 1, 
    alpha = 1) +
  labs(
    title = "Yearly Water Usage by Use Type",
    x = "Year",  
    y = "Log Gallons", 
    color = "Use Type"
  ) +
  scale_color_manual(
    values = c("Agricultural" = "black",
               "Irrigation" = "#E69F00",
               "Water Supplier" = "#56B4E9",
               "Industrial" = "#009E73",
               "Power" = "#CC79A7",
               "Domestic" = "#0072B2",
               "Commercial" = "#D55E00"),
    breaks = c("Agricultural", "Irrigation", "Water Supplier", "Industrial", "Power", "Commercial", "Domestic")
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),
    plot.background = element_rect(fill = "white", color = NA)
  )

print(plot1)

# save in higher resolution
ggsave(
  filename = "wateruse_by_type.png",
  plot = plot1,
  height = 7,
  width = 10,
  units = "in", 
  dpi = 300,
)

#------------------------------------------------------------------------------#
# GSL plot ----

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

#------------------------------------------------------------------------------#
# Water use per capita plot ----

# to plot yearly per capita usage for each use type, use yearly_per_use dataset
# to plot yearly per capita usage across all use types, use yearly_total_use dataset

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

#------------------------------------------------------------------------------#
# Precipitation vs water usage plot ----

wateruse_precip_plot = ggplot(yearly_per_use, aes(x = precip_in, y = log(year_gallons), color = use_type)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  facet_wrap(~use_type) +
  scale_color_manual(
    values = c(
      "Agricultural" = "black",
      "Irrigation" = "#E69F00",
      "Water Supplier" = "#56B4E9",
      "Industrial" = "#009E73",
      "Power" = "#CC79A7",
      "Domestic" = "#0072B2",
      "Commercial" = "#D55E00"
    )
  ) +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(
    x = "Precipitation (Inches)",
    y = "Log Gallons",
    title = "Precipitation vs. Log Water Usage"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),
    plot.background = element_rect(fill = "white", color = NA)
  )

print(wateruse_precip_plot)

# save in higher resolution
ggsave(
  filename = "wateruse_precip_plot.png",
  plot = wateruse_precip_plot,
  height = 7,
  width = 8.5,
  units = "in", 
  dpi = 300,
)

#------------------------------------------------------------------------------#
# Agricultural water use vs GSL plot ----

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

#------------------------------------------------------------------------------#

# yearly total water usage per use type
wateruse_yearly2 = masterdata |> 
  select(year, use_type, year_gallons) |> 
  mutate(use_type = ifelse(use_type == "Irrigation", "Agricultural", use_type)) |> 
  group_by(year, use_type) |> 
  summarize(
    year_gallons = sum(year_gallons)
  )

# merge yearly datasets into one
yearly_merge1_2 = left_join(wateruse_yearly2, gsl_yearly, by = "year", relationship = "many-to-one")
yearly_merge2_2 = left_join(yearly_merge1_2, pop_yearly, by = "year", relationship = "many-to-one")
yearly_merge3_2 = left_join(yearly_merge2_2, precip_yearly, by = "year", relationship = "many-to-one")

# yearly usage data for each use type
yearly_per_use2 = yearly_merge3_2 |> 
  # calculate yearly per capita water usage for each use type
  mutate(
    percapita_usage = year_gallons / population
  )

plot1_2 = ggplot() +
  geom_line(
    data = yearly_per_use2[yearly_per_use2$use_type != "Agricultural", ], 
    aes(x = year, y = log(year_gallons), color = use_type), 
    size = 0.5, 
    alpha = 0.55) +
  geom_line(
    data = yearly_per_use2[yearly_per_use2$use_type == "Agricultural", ], 
    aes(x = year, y = log(year_gallons), color = use_type), 
    size = 1, 
    alpha = 1) +
  labs(
    title = "Yearly Water Usage by Use Type",
    x = "Year",  
    y = "Log Gallons", 
    color = "Use Type"
  ) +
  scale_color_manual(
    values = c("Agricultural" = "black",
               "Water Supplier" = "#56B4E9",
               "Industrial" = "#009E73",
               "Power" = "#CC79A7",
               "Domestic" = "#0072B2",
               "Commercial" = "#D55E00"),
    breaks = c("Agricultural", "Water Supplier", "Industrial", "Power", "Commercial", "Domestic")
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),
    plot.background = element_rect(fill = "white", color = NA)
  )

print(plot1_2)

irrigation = masterdata |> 
  filter(use_type == "Irrigation") |> 
  distinct(system_name)
