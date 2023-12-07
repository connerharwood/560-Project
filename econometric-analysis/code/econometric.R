library(dplyr)
library(tidyverse)

load("~/560-Project/econometric-analysis/data/reg_data2014.rds")
load("~/560-Project/econometric-analysis/data/reg_data2022.rds")
load("~/560-Project/econometric-analysis/data/total_data2014.rds")
load("~/560-Project/econometric-analysis/data/total_data2022.rds")

reg1 = lm(gsl_volume_change ~ 
            agricultural + 
            commercial + 
            domestic + 
            industrial + 
            irrigation + 
            power + 
            water_supplier +
            population +
            precip_in,
          data = reg_data2014)
summary(reg1)

reg2 = lm(gsl_volume_change ~
            month_gallons_change +
            population +
            precip_change,
          data = reg_data)
summary(reg2)
