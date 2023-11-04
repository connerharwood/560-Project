library(dplyr)
library(tidyverse)

precip_boxElder = read_csv("precip_boxElder.csv", skip = 3, col_names = TRUE)
precip_cache = read_csv("precip_Cache.csv", skip = 3, col_names = TRUE)
precip_davis = read_csv("precip_Davis.csv", skip = 3, col_names = TRUE)
precip_morgan = read_csv("precip_Morgan.csv", skip = 3, col_names = TRUE)
precip_rich = read_csv("precip_Rich.csv", skip = 3, col_names = TRUE)
precip_saltLake = read_csv("precip_SaltLake.csv", skip = 3, col_names = TRUE)
precip_tooele = read_csv("precip_Tooele.csv", skip = 3, col_names = TRUE)
precip_utah = read_csv("precip_Utah.csv", skip = 3, col_names = TRUE)
precip_weber = read_csv("precip_Weber.csv", skip = 3, col_names = TRUE)

save(precip_boxElder, 
     precip_cache, 
     precip_davis, 
     precip_morgan, 
     precip_rich, 
     precip_saltLake,
     precip_tooele,
     precip_utah,
     precip_weber, file = "precip_raw.rda")
