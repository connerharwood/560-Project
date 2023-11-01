library(tidyverse)

# Load Utah water use data downloaded from here:
# https://waterrights.utah.gov/asp_apps/generalWaterUse/WaterUseList.asp

# Each .csv file has another table of data with different columns starting after the n_max line
# This data contains info on type of use and latitude/longitude, and is stored in the waterUse_county_info files
# This data will be merged with the waterUse_couny files

# Box Elder County
waterUse_boxelder = read_csv("wateruse_boxelder.csv", skip = 1, col_names = TRUE, n_max = 1064)
waterUse_boxelder_info = read_csv("wateruse_boxelder.csv", skip = 1067, col_names = TRUE)

# Cache County
waterUse_cache = read_csv("wateruse_cache.csv", skip = 1, col_names = TRUE, n_max = 1467)
waterUse_cache_info = read_csv("wateruse_cache.csv", skip = 1470, col_names = TRUE)

# Davis County
waterUse_davis = read_csv("wateruse_davis.csv", skip = 1, col_names = TRUE, n_max = 1114)
waterUse_davis_info = read_csv("wateruse_davis.csv", skip = 1117, col_names = TRUE)

# Morgan County
waterUse_morgan = read_csv("wateruse_morgan.csv", skip = 1, col_names = TRUE, n_max = 462)
waterUse_morgan_info = read_csv("wateruse_morgan.csv", skip = 465, col_names = TRUE)

# Rich County
waterUse_rich = read_csv("wateruse_rich.csv", skip = 1, col_names = TRUE, n_max = 284)
waterUse_rich_info = read_csv("wateruse_rich.csv", skip = 287, col_names = TRUE)

# Salt Lake County
waterUse_saltlake = read_csv("wateruse_saltlake.csv", skip = 1, col_names = TRUE, n_max = 3086)
waterUse_saltlake_info = read_csv("wateruse_saltlake.csv", skip = 3089, col_names = TRUE)

# Tooele County
waterUse_tooele = read_csv("wateruse_tooele.csv", skip = 1, col_names = TRUE, n_max = 1309)
waterUse_tooele_info = read_csv("wateruse_tooele.csv", skip = 1312, col_names = TRUE)

# Utah County
waterUse_utah = read_csv("wateruse_utah.csv", skip = 1, col_names = TRUE, n_max = 2566)
waterUse_utah_info = read_csv("wateruse_utah.csv", skip = 2569, col_names = TRUE)

# Weber County
waterUse_weber = read_csv("wateruse_weber.csv", skip = 1, col_names = TRUE, n_max = 1178)
waterUse_weber_info = read_csv("wateruse_weber.csv", skip = 1181, col_names = TRUE)

# Use rbind to append waterUse_county dataframes
waterUse_raw = rbind(waterUse_boxelder, waterUse_cache, waterUse_davis, waterUse_morgan,
                     waterUse_rich, waterUse_saltlake, waterUse_tooele, waterUse_utah,
                     waterUse_weber)

# Use rbind to append waterUse_county_info dataframes
waterUse_info = rbind(waterUse_boxelder_info, waterUse_cache_info, waterUse_davis_info, waterUse_morgan_info,
                      waterUse_rich_info, waterUse_saltlake_info, waterUse_tooele_info, waterUse_utah_info,
                      waterUse_weber_info)

# save to .rda file
save(waterUse_raw, waterUse_info,
     file = "waterUse_raw.rda")
