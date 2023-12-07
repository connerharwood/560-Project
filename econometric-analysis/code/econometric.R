library(dplyr)
library(tidyverse)

load("~/560-Project/econometric-analysis/data/reg_data2014.rds")
load("~/560-Project/econometric-analysis/data/reg_data2022.rds")
load("~/560-Project/econometric-analysis/data/total_data2014.rds")
load("~/560-Project/econometric-analysis/data/total_data2022.rds")

#------------------------------------------------------------------------------#
# 1996-2014 regression by use type
reg2014 = lm(gsl_volume_change ~ 
            lag(agricultural, 12) + 
            lag(commercial, 12) + 
            lag(domestic, 12) + 
            lag(industrial, 12) + 
            lag(irrigation, 12) + 
            lag(power, 12) + 
            lag(water_supplier, 12) +
            lag(pop_change, 12) +
            lag(precip_change, 12),
          data = reg_data2014)
summary(reg2014)

#------------------------------------------------------------------------------#
# 1996-2022 regression by use type

reg2022 = lm(gsl_volume_change ~
            month_gallons_change +
            population +
            precip_change,
          data = reg_data)
summary(reg2022)

#------------------------------------------------------------------------------#
# 1996-2014 regression by total use

#------------------------------------------------------------------------------#
# 1996-2022 regression by total use