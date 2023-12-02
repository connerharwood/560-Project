library(dplyr)
library(tidyverse)
library(readxl)

wateruse_info = read_csv("wateruse_boxelder.csv", skip = 1, col_names = TRUE, n_max = 1064)
wateruse = read_csv("wateruse_boxelder.csv", skip = 1067, col_names = TRUE)