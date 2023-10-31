library(dataRetrieval)
library(dplyr)
library(tidyverse)
library(foreign)
library(ggplot2)

# set appropriate working directory (IN CONSOLE) to shared "Project" folder

# Conner's desktop: setwd("C:/Users/h85h998/OneDrive - Montana State University/Project")
# Conner's laptop: 

# Great Salt Lake North Arm daily levels
GSL_northLevels = readNWISdv(
  siteNumbers = "10010100", # gauge near Saline, UT
  parameterCd = "62614", # lake elevation parameter code
  startDate = "1966-04-15",
  endDate = ""
)

# Great Salt Lake South Arm daily levels
GSL_southLevels = readNWISdv(
  siteNumbers = "10010000", # gauge at Saltair Boat Harbor
  parameterCd = "62614", # lake elevation parameter code
  startDate = "1966-04-15",
  endDate = ""
)

# instantaneous discharge value in one-hour intervals at start of Bear River
bearRiver_start = readNWISuv(
  siteNumbers = "10011500", # Bear River near UT-WY state line
  parameterCd = "00060", # discharge parameter code
  startDate = "1986-10-02",
  endDate = ""
)

# instantaneous discharge value in one-hour intervals at end of Bear River
bearRiver_end = readNWISuv(
  siteNumbers = "10126000", # Bear River near Corinne, UT
  parameterCd = "00060", # discharge parameter code
  startDate = "1986-10-02",
  endDate = ""
) 

# instantaneous discharge value in one-hour intervals near start of Weber River
weberRiver_start = readNWISuv(
  siteNumbers = "10128500", # Weber River near Oakley, UT
  parameterCd = "00060", # discharge parameter code
  startDate = "1986-10-02",
  endDate = ""
)

# instantaneous discharge value in one-hour intervals at end of Weber River
weberRiver_end = readNWISuv(
  siteNumbers = "10141000", # Weber River near Plain City, UT
  parameterCd = "00060", # discharge parameter code
  startDate = "1986-10-02",
  endDate = ""
) 

# read in water-related land use data
landUse1986_1992 = read.csv("1986-1992.csv")
landUse1989_1999 = read.csv("1989-1999.csv")
landUse2000_2005 = read.csv("2000-2005.csv")
landUse2005_2010 = read.csv("2005-2010.csv")
landUse2010_2015 = read.csv("2010-2015.csv")
landUse2017 = read.csv("2017.csv")
landUse2018 = read.csv("2018.csv")
landUse2019 = read.csv("2019.csv")
landUse2020 = read.csv("2020.csv")
landUse2021 = read.csv("2021.csv")
landUse2022 = read.csv("2022.csv")

# 2016 water-related land use data (function package'foreign')
landUse2016 = read.dbf("StateWideLandUse2016.dbf")

# read in water use data
waterUse = read.csv("water_use_data.csv", skip = 1)

# save all data to R data file
save(GSL_northLevels,  GSL_southLevels,
     bearRiver_start,  bearRiver_end,
     weberRiver_start, weberRiver_end,
     landUse1986_1992,
     landUse1989_1999,
     landUse2000_2005,
     landUse2005_2010,
     landUse2010_2015,
     landUse2016,
     landUse2017,
     landUse2018,
     landUse2019,
     landUse2020,
     landUse2021,
     landUse2022,
     waterUse,
     file = "rawData.rda"
)
