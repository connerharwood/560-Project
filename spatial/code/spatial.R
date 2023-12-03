library(sf)
library(tidyverse)
library(dplyr)

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
# Map plot ----

# plot GSL Basin with subbasins on Utah map with county outlines
basin_plot = ggplot() +
  # plot Utah counties shapefile
  geom_sf(
    data = counties,
    color = "gray55", 
    fill = "gray90"
  ) +
  # plot GSL Basin shapefile
  geom_sf(
    data = gsl_basin,
    aes(fill = subbasin), 
    color = "black"
  ) +
  # fill and properly label GSL subbasins
  scale_fill_manual(
    values = c("skyblue1", "salmon", "seagreen", "mediumpurple", "navajowhite"),
    breaks = c("Great Salt Lake", "Bear", "Jordan/Provo", "Weber", "West Desert"),
    labels = c("Great Salt Lake", "Bear River Subbasin", "Jordan/Provo River Subbasin", "Weber River Subbasin", "West Desert Subbasin")
  ) +
  theme_minimal() +
  # remove grids and coordinates
  theme_void() +
  # remove legend title, add plot title
  labs(fill = "", title = "Great Salt Lake Basin") +
  theme(
    # adjust legend position to be in missing top right corner of Utah on png image
    legend.position = c(0.9348, 1.001),
    legend.justification = c(1, 1),
    legend.margin = margin(t = 10, r = 0, b = 0, l = 0),
    # adjust legend size
    legend.key.size = unit(0.6, "cm"),
    # adjust legend text size
    legend.text = element_text(size = 8.2),
    # center and move title down
    plot.title = element_text(hjust = 0.5, size = 15, margin = margin(b = -15)),
    # add white space to top so title isn't touching top of png image
    plot.margin = margin(t = 17, r = 0, b = 0, l = 0),
    # add white background for png image
    plot.background = element_rect(fill = "white", color = NA)
  )

# show plot
print(basin_plot)
# the plot looks weird in R because I had to adjust the legend and margins to look good on the png image

# save in higher resolution
ggsave(
  filename = "basin_plot.png",
  plot = basin_plot,
  units = "in", 
  dpi = 300,
)
