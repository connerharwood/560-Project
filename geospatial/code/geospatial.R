library(sf)
library(tmap)
library(tidyverse)

#------------------------------------------------------------------------------#
# counties ----

# read in Utah counties shapefile
counties = st_read("geospatial/utah counties/utahcounties.shp")

# cleaning
counties = counties |> 
  select(
    color = color4,
    county_number = countynbr,
    fips,
    name,
    shape_area,
    shape_length = shape_leng,
    stateplane,
    geometry
  ) |> 
  mutate(name = str_to_title(name))

plot(counties["name"])

#------------------------------------------------------------------------------#
# GSL Basin ----

# read in GSL Basin shapefile
gsl_basin = st_read("geospatial/gsl basin/GSLSubbasins.shp")

# cleaning
gsl_basin = gsl_basin |> 
  select(
    grid_code = GRIDCODE,
    subbasin = Name,
    shape_area = Shape_Area,
    shape_length = Shape_Leng,
    geometry
  )

plot(gsl_basin["subbasin"])
