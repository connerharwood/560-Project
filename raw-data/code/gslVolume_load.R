library(tidyverse)

# read in csv file with monthly Great Salt Lake volume data
gsl_volume = read_csv("gsl_volume.csv", col_names = TRUE)

# select relevant variables
gsl_volume_raw = gsl_volume |> 
  select(date = Date,
         volume_m3 = Total_vol_m3)

# save as .rds file
save(gsl_volume_raw, file = "gslVolume_raw.rds")