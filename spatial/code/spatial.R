library(sf)
library(tmap)
library(tidyverse)
library(dplyr)
library(patchwork)

#------------------------------------------------------------------------------#
# Counties ----

# read in Utah counties shapefile
counties = st_read("spatial/data/counties/utahcounties.shp")

# cleaning
counties = counties |> 
  select(
    county_num = countynbr,
    county = name,
    shape_area,
    shape_length = shape_leng,
    stateplane,
    geometry
  ) |> 
  mutate(county = str_to_title(county))

# load("~/560-Project/clean-data/data/masterdata.rds")
# 
# # relevant counties to plot
# counties = masterdata |> 
#   distinct(county) |> 
#   arrange(county) |> 
#   pull(county)
# 
# # filter to include only relevant counties in shapefile
# relevant_counties_sf = all_counties_sf |> 
#   filter(county %in% counties)
# 
# plot(all_counties_sf["county"])

#------------------------------------------------------------------------------#
# GSL Basin ----

# read in GSL Basin shapefile
gsl_basin = st_read("spatial/data/basin/GSLSubbasins.shp")

# cleaning
gsl_basin = gsl_basin |> 
  select(
    grid_code = GRIDCODE,
    subbasin = Name,
    shape_area = Shape_Area,
    shape_length = Shape_Leng,
    geometry
  ) |> 
  filter(subbasin != "Strawberry")

# transform GSL Basin simple feature to have same CRS as counties simple feature
gsl_basin = st_transform(gsl_basin, st_crs(counties))

# include only the part of GSL Basin lying within Utah
gsl_basin = st_intersection(gsl_basin, st_union(counties))


plot(gsl_basin["subbasin"])

#------------------------------------------------------------------------------#
# Map overlay ----

# plot GSL Basin with subbasins on Utah map with county outlines
basin_plot = ggplot() +
  geom_sf(
    data = counties, 
    color = "gray60", 
    fill = "gray95"
  ) +
  geom_sf(
    data = gsl_basin, 
    aes(fill = subbasin), 
    color = "black"
  ) +
  scale_fill_manual(
    values = c("skyblue1", "salmon", "seagreen", "mediumpurple", "navajowhite"),
    breaks = c("Great Salt Lake", "Bear", "Jordan/Provo", "Weber", "West Desert"),
    labels = c("Great Salt Lake", "Bear River Subbasin", "Jordan/Provo River Subbasin", "Weber River Subbasin", "West Desert Subbasin")
  ) +
  theme_minimal() +
  theme_void() +
  labs(fill = "", title = "Great Salt Lake Basin") +
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = 0.5, size = 15, margin = margin(b = -10))
  )

# original position for legend: c(1.023, 0.99)

# show plot
print(basin_plot)

# save in higher resolution, maintain legend position
ggsave(
  filename = "basin_plot.png",
  plot = basin_plot,
  width = 5, 
  height = 3, 
  units = "in", 
  dpi = 300,
)

# For some reason when I save from the "Plots" tab, the image is in very low resolution, so I used ggsave to specifiy
# higher resolution.
# When saving this way, the legend kept being moved out of place, so I screenshot a picture of the legend, saved
# a plot without the legend, and photoshopped the legend into its proper position
# Not the most ideal way to go about it, but I could not figure out how to keep it in the right spot

# code for legend (removed to save a plot without the legend for photoshopping):

# theme(
#   legend.position = c(1.022, 0.99),
#   legend.justification = c(1, 1),
#   legend.margin = margin(t = 0, r = 10, b = 0, l = 0),
#   legend.key.size = unit(0.43, "cm"),
#   legend.text = element_text(size = 7.3),
#   plot.title = element_text(hjust = 0.5, size = 15, margin = margin(b = -15))
# )