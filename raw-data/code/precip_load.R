library(dplyr)
library(tidyverse)

precip_raw = read_csv("precipitation_data")

save(precip_raw, file = "precip_raw.rds")
