library(dplyr)
library(zoo)
library(tidyverse)


library(binsreg)

library(scales)

library(lubridate)

library(cowplot)

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
# Water usage by use type plot ----

wateruse_by_type_plot = ggplot() +
  # make lines non-agricultural use types thinner and faded
  geom_line(
    data = yearly_per_use[yearly_per_use$use_type != "Agricultural", ], 
    aes(x = year, y = log(year_gallons), color = use_type), 
    size = 0.5, 
    alpha = 0.6) +
  # make lines for agricultural use type thicker
  geom_line(
    data = yearly_per_use[yearly_per_use$use_type == "Agricultural", ], 
    aes(x = year, y = log(year_gallons), color = use_type), 
    size = 1, 
    alpha = 1) +
  labs(
    title = "Yearly Water Use by Use Type",
    x = "Year",  
    y = "Log Gallons", 
    color = "Use Type"
  ) +
  # select colors and manually select order of legend
  scale_color_manual(
    values = c("Agricultural" = "black",
               "Irrigation" = "#E69F00",
               "Water Supplier" = "#56B4E9",
               "Industrial" = "#009E73",
               "Power" = "#CC79A7",
               "Commercial" = "#D55E00",
               "Domestic" = "#0072B2"),
    breaks = c("Agricultural", "Irrigation", "Water Supplier", "Industrial", "Power", "Commercial", "Domestic")
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),
    plot.background = element_rect(fill = "white", color = NA)
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
    title = "Water Use vs Precipitation"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),
    plot.background = element_rect(fill = "white", color = NA)
  )

print(wateruse_precip_plot)

# save as higher resolution png image
ggsave(
  filename = "wateruse_precip_plot.png",
  plot = wateruse_precip_plot,
  height = 7,
  width = 8.5,
  units = "in", 
  dpi = 300,
)

#------------------------------------------------------------------------------#
# Ag water use vs GSL volume plot ----

# select only agricultural water use
ag_use = yearly_per_use |> 
  filter(use_type == "Agricultural")

# create plot with ag water use and GSL volume
ag_gsl_plot = ggplot(ag_use, aes(x = year)) +
  # convert GSL volume to one trillion gallons (later converted to one hundred billion gallons in sec.axis), plot
  geom_line(aes(y = gsl_volume_gal/1e12, color = "GSL Volume"), size = 1.2) +
  # convert ag water use to one hundred billion gallons, plot
  geom_line(aes(y = year_gallons/1e11, color = "Ag Water Use"), size = 1.2) +
  # rename axes and title
  labs(
    x = "Year",
    y = "GSL Volume (Gallons)",
    title = "Agricultural Water Use and GSL Volume"
  ) +
  # choose colors for lines
  scale_color_manual(values = c("GSL Volume" = "lightblue", "Ag Water Use" = "pink")) +
  theme_minimal() +
  # center title
  theme(plot.title = element_text(hjust = 0.5)) +
  # position legend to bottom of graph
  theme(legend.position = "bottom") +
  # create separate y-axes for ag water use and GSL volume
  scale_y_continuous(
    name = "Ag Water Use (100Bn Gallons)",
    sec.axis = sec_axis(~.*10, name = "GSL Volume (100Bn Gallons)")
  ) +
  # remove vertical grids
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()
  ) +
  # remove legend title
  labs(color = NULL) +
  # add horizontal line indicating minimum GSL volume for a healthy lake
  geom_hline(yintercept = 4.475119e+12/1e12, linetype = "dashed", color = "blue") + 
  geom_text(aes(x = 2015.5, y = 4.65, label = "Min Healthy Lake Volume = 44.75"), color = "blue", hjust = 1, size = 3)

print(ag_gsl_plot)

# save as higher resolution png image
ggsave(
  filename = "ag_gsl_plot.png",
  plot = wateruse_precip_plot,
  height = 7,
  width = 8.5,
  units = "in",
  dpi = 300,
)
