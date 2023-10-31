library(dplyr)
library(tidyr)

GSL_northLevels = GSL_northLevels |> 
  mutate(north_levels = X_62614_00003) |>
  select(-X_62614_00003, 
         -agency_cd,
         -site_no,
         -X_62614_00003_cd)

GSL_southLevels = GSL_southLevels |> 
  mutate(south_levels = X_62614_00003) |>
  select(-X_62614_00003,
         -agency_cd,
         -site_no,
         -X_62614_00003_cd)

gsl_levels = merge(GSL_northLevels, GSL_southLevels, by = "Date")

gsl_levels$north_levels = as.numeric(gsl_levels$north_levels)
gsl_levels$south_levels = as.numeric(gsl_levels$south_levels)

gsl_levels = gsl_levels |> 
  mutate(level = (north_levels + south_levels) / 2)

gsl_levels$year = format(gsl_levels$Date, "%Y")

gsl_levels <- aggregate(level ~ year, gsl_levels, FUN = mean)


