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
               "Irrigation" = "#56B4E9",
               "Water Supplier" = "#E69F00",
               "Industrial" = "#009E73",
               "Power" = "#CC79A7",
               "Commercial" = "#D55E00",
               "Domestic" = "#0072B2"),
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
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.line = element_line(color = "gray", size = 0.2)
  )

print(wateruse_by_type_plot)

# save as higher resolution png image
ggsave(
  filename = "wateruse_by_type_plot.png",
  plot = wateruse_by_type_plot,
  height = 7,
  width = 10,
  units = "in", 
  dpi = 300,
)

#------------------------------------------------------------------------------#
# GSL plot ----

gsl_monthly = masterdata |> 
  group_by(year, month) |> 
  summarize(gsl_level_ft = mean(gsl_level_ft)) |> 
  mutate(
    date = as.Date(paste(year, month, "01"), format = "%Y %b %d"),
    date = as.yearmon(date)
  ) |> 
  select(date, gsl_level_ft)

# create a graph showing GSL levels over time 
gsl_plot = ggplot(gsl_monthly, aes(x = date, y = gsl_level_ft)) + 
  geom_smooth(method = "loess", span = 0.12, color = "cornflowerblue", se = FALSE) +
  geom_hline(yintercept = 4198, linetype = "dashed", color = "coral3") +
  geom_text(aes(x = max(year), y = 4198.3, label = "Minimum Healthy Lake Level", color = "coral3"), hjust = 1) +
  labs( 
    title = "Great Salt Lake Water Level",
    x = "Year", 
    y = "Feet") + 
  theme_minimal() +
  guides(color = "none") +
  theme(
    # center and resize title
    plot.title = element_text(hjust = 0.5, size = 15),
    # create white background for png image
    plot.background = element_rect(fill = "white", color = NA),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()
  ) +
  scale_x_yearmon(format = "%Y", n = 9) +
  scale_y_continuous(breaks = seq(4194, 4204, by = 2))

print(gsl_plot)

#------------------------------------------------------------------------------#

gsl_monthly2 = gsl_clean |> 
  mutate(
    date = as.Date(paste(year, month, "01"), format = "%Y %b %d"),
    date = as.yearmon(date)
  ) |> 
  select(date, level)

# create a graph showing GSL levels over time 
gsl_plot2 = ggplot(gsl_monthly2, aes(x = date, y = level)) + 
  geom_smooth(method = "loess", span = 0.08, color = "cornflowerblue", se = FALSE) +
  geom_hline(yintercept = 4198, linetype = "dashed", color = "coral3") +
  geom_vline(xintercept = 1996, linetype = "solid", color = "black") +
  geom_vline(xintercept = 2014, linetype = "solid", color = "black") +
  geom_text(aes(x = max(date), y = 4198.5, label = "Minimum Healthy Lake Level", color = "coral3"), hjust = 0.9) +
  geom_text(x = 2005, y = 4200, label = "___ Study Period ___", color = "black", vjust = -17) +
  labs( 
    title = "Great Salt Lake Water Level",
    x = "Year", 
    y = "Feet") + 
  theme_minimal() +
  guides(color = "none") +
  theme(
    # center and resize title
    plot.title = element_text(hjust = 0.5, size = 15),
    # create white background for png image
    plot.background = element_rect(fill = "white", color = NA),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()
  ) +
  scale_x_yearmon(format = "%Y", n = 9) +
  scale_y_continuous(breaks = seq(4188, 4212, by = 2))

print(gsl_plot2)

# save as higher resolution png image
ggsave(
  filename = "gsl_plot.png",
  plot = gsl_plot,
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

#------------------------------------------------------------------------------#
# Water use histogram ----

# convert month and year to date format
masterdata_monthly = masterdata |> 
  mutate(
    date = as.Date(paste(year, month, "01"), format = "%Y %b %d"),
    date = as.yearmon(date)
  )

# monthly total water usage per use type
wateruse_monthly = masterdata_monthly |>
  select(date, use_type, month_gallons) |> 
  group_by(date, use_type) |> 
  summarize(
    month_gallons = sum(month_gallons)
  )

# histogram of water use by use type
wateruse_hist = ggplot(wateruse_monthly, aes(x = month_gallons/1e10, color = use_type, fill = use_type)) +
  geom_histogram(bins = 100000) +
  facet_wrap(~use_type)
print(wateruse_hist)
