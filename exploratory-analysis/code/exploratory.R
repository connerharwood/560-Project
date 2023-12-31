library(dplyr)
library(zoo)
library(tidyverse)

load("~/560-Project/clean-data/data/masterdata.rds")

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
# Aggregate yearly data for 2015-2022 period ----

load("~/560-Project/clean-data/data/wateruse_1996_2022.rds")

# yearly total water use per use type
wateruse2015_2022 = wateruse_1996_2022 |> 
  filter(year >= 2015) |> 
  select(year, use_type, year_gallons) |> 
  group_by(year, use_type) |> 
  summarize(
    year_gallons = sum(year_gallons)
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
  ) 

#------------------------------------------------------------------------------#
# Water use by use type plot ----

wateruse_by_type_plot = ggplot() +
  geom_line(
    # plot non-agricultural use types
    data = yearly_per_use[yearly_per_use$use_type != "Agricultural", ], 
    aes(x = year, y = log(year_gallons), color = use_type), 
    # decrease size of non-agricultural use type lines and fade
    size = 0.5, 
    alpha = 0.5
  ) +
  geom_line(
    # plot agricultural use type
    data = yearly_per_use[yearly_per_use$use_type == "Agricultural", ], 
    aes(x = year, y = log(year_gallons), color = use_type), 
    # increase size of agricultural use type and don't fade
    size = 1.2, 
    alpha = 1
  ) +
  labs(
    title = "Yearly Water Use by Use Type",
    x = "Year",  
    y = "Log Gallons", 
    color = "Use Type"
  ) +
  scale_color_manual(
    # select colors for each use type line
    values = c("Agricultural" = "black",
               "Water Supplier" = "#E69F00",
               "Industrial" = "#56B4E9",
               "Irrigation" = "#009E73",
               "Power" = "#CC79A7",
               "Domestic" = "#D55E00",
               "Commercial" = "#0072B2"),
    # manually select order of legend by descending log water use in last year on plot
    breaks = c("Agricultural", "Water Supplier", "Industrial", "Irrigation", "Power", "Domestic", "Commercial")
  ) +
  scale_x_continuous(breaks = seq(1996, 2014, by = 2)) +
  theme_minimal() +
  theme(
    # center and resize title
    plot.title = element_text(hjust = 0.5, size = 15),
    # create white background for png image
    plot.background = element_rect(fill = "white", color = NA), 
    # remove grid lines 
    panel.grid.major.x = element_blank(),  
    panel.grid.minor.x = element_blank(), 
    panel.grid.major.y = element_blank(), 
    panel.grid.minor.y = element_blank(), 
    axis.line = element_line(color = "gray", size = 0.2)
  )

print(wateruse_by_type_plot)

# save as higher resolution png image
ggsave(
  filename = "wateruse_by_type.png",
  plot = wateruse_by_type_plot,
  height = 7,
  width = 10,
  units = "in", 
  dpi = 300,
)

#------------------------------------------------------------------------------#
# Water use by use type plot for 2015-2022 ----

wateruse_by_type_plot2022 = ggplot() +
  geom_line(
    # plot non-agricultural use types
    data = wateruse2015_2022[wateruse2015_2022$use_type != "Agricultural", ], 
    aes(x = year, y = log(year_gallons), color = use_type), 
    # decrease size of non-agricultural use type lines and fade
    size = 0.5, 
    alpha = 0.5
  ) +
  geom_line(
    # plot agricultural use type
    data = wateruse2015_2022[wateruse2015_2022$use_type == "Agricultural", ], 
    aes(x = year, y = log(year_gallons), color = use_type), 
    # increase size of agricultural use type and don't fade
    size = 1.2, 
    alpha = 1
  ) +
  labs(
    title = "Yearly Water Use by Use Type (2015-2022)",
    x = "Year",  
    y = "Log Gallons", 
    color = "Use Type"
  ) +
  scale_color_manual(
    # select colors for each use type line
    values = c("Agricultural" = "black",
               "Water Supplier" = "#E69F00",
               "Industrial" = "#56B4E9",
               "Irrigation" = "#009E73",
               "Power" = "#CC79A7",
               "Domestic" = "#D55E00",
               "Commercial" = "#0072B2"),
    # manually select order of legend by descending log water use in last year on plot
    breaks = c("Agricultural", "Water Supplier", "Industrial", "Irrigation", "Power", "Domestic", "Commercial")
  ) +
  scale_x_continuous(breaks = seq(2015, 2022, by = 1)) +
  theme_minimal() +
  theme(
    # center and resize title
    plot.title = element_text(hjust = 0.5, size = 15),
    # create white background for png image
    plot.background = element_rect(fill = "white", color = NA), 
    # remove grid lines 
    panel.grid.major.x = element_blank(),  
    panel.grid.minor.x = element_blank(), 
    panel.grid.major.y = element_blank(), 
    panel.grid.minor.y = element_blank(), 
    axis.line = element_line(color = "gray", size = 0.2)
  )

print(wateruse_by_type_plot2022)

# save as higher resolution png image
ggsave(
  filename = "wateruse_by_type2022.png",
  plot = wateruse_by_type_plot2022,
  height = 7,
  width = 10,
  units = "in", 
  dpi = 300,
)

#------------------------------------------------------------------------------#
# Lake volume and total water use plot ----

water_use_gsl_plot = ggplot(yearly_total_use, aes(x = year)) +
  # convert GSL volume to one trillion gallons (later converted to one hundred billion gallons in sec.axis), plot
  geom_line(aes(y = gsl_volume_gal/1e12, color = "GSL Volume"), size = 1.2) +
  # convert ag water use to one hundred billion gallons, plot
  geom_line(aes(y = year_gallons/1e12, color = "Total Water Use"), size = 1.2) +
  # rename axes and title
  labs(
    x = "Year",
    y = "Trillions of Gallons",
    title = "Total Water Use and GSL Volume",
    color = NULL
  ) +
  # choose colors for lines
  scale_color_manual(values = c("GSL Volume" = "lightblue", "Total Water Use" = "pink")) +
  scale_x_continuous(breaks = seq(1996, 2014, by = 2)) +
  theme_minimal() +
  theme(
    # center and resize title
    plot.title = element_text(hjust = 0.5, size = 15),
    # moving legend
    legend.position = "bottom",
    # create white background for png image
    plot.background = element_rect(fill = "white", color = NA),
    # remove vertical gridlines
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()
  ) +
  # add horizontal line indicating minimum GSL volume for a healthy lake
  geom_hline(
    yintercept = 4.475119e+12/1e12,
    linetype = "dashed",
    color = "blue") +
  geom_text(
    aes(x = 2015.5, y = 4.35, label = "Min Healthy Lake Volume = 4.475"),
    color = "blue",
    hjust = 4,
    size = 3
  )

print(water_use_gsl_plot)

# save as higher resolution png image
ggsave(
  filename = "water_use_gsl_plot.png",
  plot = water_use_gsl_plot,
  height = 6,
  width = 8.5,
  units = "in",
  dpi = 300,
)

#------------------------------------------------------------------------------#
# Water use vs precipitation plot ----

wateruse_precip_plot = ggplot(yearly_per_use, aes(x = precip_in, y = log(year_gallons), color = use_type)) +
  # scatterplot of water use vs precipitation
  geom_point() +
  # add line of best fit to each panel plot
  geom_smooth(method = "lm", se = FALSE) +
  # individual panel plots for each water use type
  facet_wrap(~use_type) +
  # select colors for each use type panel plot
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
  # remove legend
  theme(legend.position = "none") +
  labs(
    x = "Precipitation (Inches)",
    y = "Log Gallons",
    title = "Water Use vs Precipitation"
  ) +
  theme(
    # center and resize title
    plot.title = element_text(hjust = 0.5, size = 15),
    # create white background for png image
    plot.background = element_rect(fill = "white", color = NA)
  )

print(wateruse_precip_plot)

# save as higher resolution png image
ggsave(
  filename = "wateruse_precip_plot.png",
  plot = wateruse_precip_plot,
  height = 6.5,
  width = 8.5,
  units = "in", 
  dpi = 300,
)
