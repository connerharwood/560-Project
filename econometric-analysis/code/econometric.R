library(dplyr)
library(tidyverse)
library(stargazer)

load("~/560-Project/econometric-analysis/data/reg_data2014.rds")
load("~/560-Project/econometric-analysis/data/reg_data2022.rds")
load("~/560-Project/econometric-analysis/data/total_data2014.rds")
load("~/560-Project/econometric-analysis/data/total_data2022.rds")

#------------------------------------------------------------------------------#
# 1996-2014 regression by use type ----
reg2014 = lm(gsl_volume_change / 1000000000 ~ 
            lag(agricultural, 12) + 
            lag(commercial, 12) + 
            lag(domestic, 12) + 
            lag(industrial, 12) + 
            lag(irrigation, 12) + 
            lag(power, 12) + 
            lag(water_supplier, 12) +
            lag(population, 12) +
            lag(precip_in, 12),
          data = reg_data2014)
summary(reg2014)

#------------------------------------------------------------------------------#
# 1996-2022 regression by use type ----

reg2022 = lm(gsl_volume_change / 1000000000 ~ 
               lag(agricultural, 12) + 
               lag(commercial, 12) + 
               lag(domestic, 12) + 
               lag(industrial, 12) + 
               lag(irrigation, 12) + 
               lag(power, 12) + 
               lag(water_supplier, 12) +
               lag(population, 12) +
               lag(precip_in, 12),
             data = reg_data2022)
summary(reg2022)

#------------------------------------------------------------------------------#
# 1996-2014 regression by total use ----

total_reg2014 = lm(gsl_volume_change / 1000000000 ~
                     lag(log_month_gallons, 12) +
                     lag(population, 12) +
                     lag(precip_in),
                   data = total_data2014)
summary(total_reg2014)

#------------------------------------------------------------------------------#
# 1996-2022 regression by total use ----

total_reg2022 = lm(gsl_volume_change / 1000000000 ~
                     lag(log_month_gallons, 12) +
                     lag(population, 12) +
                     lag(precip_in),
                   data = total_data2022)
summary(total_reg2022)

#------------------------------------------------------------------------------#
# Regression output table ----

models_by_use = list(reg2014, reg2022)

stargazer(models_by_use, title = "Regression Results", align = TRUE)

models_total_use = list(total_reg2014, total_reg2022)

stargazer(models_total_use, title = "Regression Results", align = TRUE)
