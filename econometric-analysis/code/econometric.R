library(dplyr)
library(tidyverse)

load("~/560-Project/econometric-analysis/data/reg_type_1996_2014.rds")
load("~/560-Project/econometric-analysis/data/reg_type_1996_2022.rds")

reg_log_data = reg_type_1996_2014 |> 
  mutate(
    ln_agricultural = log(agricultural/100),
    ln_commercial = log(commercial/100),
    ln_domestic = log(domestic/100),
    ln_industrial = log(industrial/100),
    ln_irrigation = log(irrigation/100),
    ln_power = log(power/100),
    ln_water_supplier = log(water_supplier/100)
  )
  
reg = lm(gsl_volume_change ~ 
            agricultural + 
            commercial + 
            domestic + 
            industrial + 
            irrigation + 
            power + 
            water_supplier +
            population +
            precip_in,
          data = reg_type_1996_2014)

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
          data = reg_type_1996_2014)
summary(reg1)

reg2 = lm(gsl_volume_change ~
            month_gallons_change +
            population +
            precip_change,
          data = reg_data)
summary(reg2)
