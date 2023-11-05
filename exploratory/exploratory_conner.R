library(dplyr)
library(tidyverse)
library(data.table)

load("~/560-Project/clean-data/data/masterData.rds")

# group similar use type categories
masterData = masterData |> 
  mutate(use_type = ifelse(use_type == "Irrigation", "Agricultural",
                           ifelse(use_type %in% c("Power (Hydro-Elec)", 
                                                  "Power (Fossil-Fuel)", 
                                                  "Power (Geothermal)",
                                                  "Geothermal"),
                                  "Power", use_type)))

masterData_table = as.data.table(masterData)

yearlyData2 = masterData_table |> 
  group_by(year, county) |> 
  summarize(total_county_usage = sum(year_gallons),
            precip = sum(county_precip))
  
yearlyData = masterData_table |> 
  group_by(year, use_type) |> 
  summarize(total = sum(year_gallons),
            gsl_level = mean(gsl_level, na.rm = TRUE)) |> 
  mutate(log_total = log(total)) |> 
  filter(year >= 1966 & use_type != "Sewage Treatment")

ggplot(yearlyData, aes(x = year)) +
  geom_line(aes(y = gsl_level, color = use_type), size = 1) +
  geom_bar(aes(y = log_total, fill = use_type), stat = "identity", position = "dodge", width = 0.5) +
  scale_y_continuous(
    name = "Great Salt Lake Levels (Feet)",
    sec.axis = sec_axis(trans = log, name = "Water Usage (Log Gallons/year)")
  ) +
  labs(
    x = "Year",
    color = "Measurement"
  ) +
  theme_minimal()

ggplot(yearlyData, aes(x = year, y = log_total, color = use_type)) +
  geom_line() +
  labs(
    x = "Year",
    y = "Log Yearly Water Usage",
    title = "Log Water Usage by Use Type Over Time",
    color = "Use Type"
  ) +
  theme_minimal()

library(ggplot2)

ggplot() +
  geom_line(data = yearlyData, aes(x = year, y = log_total, color = factor(use_type, levels = ), size = 1) +
  geom_line(data = yearlyData, aes(x = year, y = gsl_level), size = 1, linetype = "dashed", color = "black") +
  labs(
    x = "Year",
    y = "Log Water Usage (Log Gallons/year) / GSL Level (Feet)",
    color = "Sector"
  ) +
  scale_color_manual()
  theme_minimal()

yearlyData_order = yearlyData |> 
  filter(year == 2022) |> 
  arrange(desc(log_total))


ggplot(yearlyData, aes(x = year, y = gsl_level)) +
  geom_line()


abc = waterUse_clean |> 
  group_by(year, county) |> 
  summarize(population = sum(population)) |> 
  ungroup()
