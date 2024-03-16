
# Initializing / Cleaning Script

# packages
library(dplyr)
library(stringr)
library(lubridate)
library(ggplot2)
library(corrplot)
library(rpivotTable)

# raw data
raw_data_energy <- read.csv("Data/energy_dataset.csv")
raw_data_weather <- read.csv("Data/weather_features.csv")

# energy dataset clean up
data_energy <- raw_data_energy %>% 
  # col names to snake case
  rename_with(~ str_replace_all(.x, "\\.", "_")) %>% 
  # add in year, month, day, weekend, season
  mutate(
    year    = year(time),
    month   = month(time),
    day     = day(time),
    hour    = hour(time),
    weekend = wday(time, label = TRUE) %in% c("Sat", "Sun"),
    season  = case_when(
      month %in% c(12, 1, 2) ~ "Winter",
      month %in% c(3, 4, 5) ~ "Spring",
      month %in% c(6, 7, 8) ~ "Summer",
      month %in% c(9, 10, 11) ~ "Fall",
      TRUE ~ NA
    )) %>% 
  # move to beginning of df
  relocate(year, month, day, hour, weekend, season, .after = time) %>% 
  # remove cols that are empty
  select(
    -generation_hydro_pumped_storage_aggregated,
    -forecast_wind_offshore_eday_ahead)

# weather dataset clean up
data_weather <- raw_data_weather %>% 
  # rename date to time for consistency
  rename(time = dt_iso) %>% 
  # add in year, month, day, weekend, season
  mutate(
    year    = year(time),
    month   = month(time),
    day     = day(time),
    hour    = hour(time),
    weekend = wday(time, label = TRUE) %in% c("Sat", "Sun"),
    season  = case_when(
      month %in% c(12, 1, 2) ~ "Winter",
      month %in% c(3, 4, 5) ~ "Spring",
      month %in% c(6, 7, 8) ~ "Summer",
      month %in% c(9, 10, 11) ~ "Fall",
      TRUE ~ NA
    )) %>% 
  # move to beginning of df
  relocate(year, month, day, hour, weekend, season, .after = time)

# averaging the weather
data_weather_average <- data_weather %>%
  group_by(time, year, month, day, hour, weekend, season) %>%
  summarise(
    city_name = "Average",
    temp       = mean(temp, na.rm = TRUE),
    temp_min   = mean(temp_min, na.rm = TRUE),
    temp_max   = mean(temp_max, na.rm = TRUE),
    pressure   = mean(pressure, na.rm = TRUE),
    humidity   = mean(humidity, na.rm = TRUE),
    wind_speed = mean(wind_speed, na.rm = TRUE),
    wind_deg   = mean(wind_deg, na.rm = TRUE),
    rain_1h    = mean(rain_1h, na.rm = TRUE),
    rain_3h    = mean(rain_3h, na.rm = TRUE),
    snow_3h    = mean(snow_3h, na.rm = TRUE),
    clouds_all = mean(clouds_all, na.rm = TRUE),
    .groups    = 'drop')

# final data set - uses avg weather data
data <- data_weather_average %>% 
  mutate(month = as.numeric(month)) %>% 
  # join weather data with energy data
  full_join(data_energy,
            by = join_by(time, year, month, day, hour, weekend, season)) %>% 
  # filter empty values - removes 47 out of 35064 rows
  filter(complete.cases(.))

# QC Check

# find missing values for energy set (there are none for weather)
data %>% summarise_all(~sum(is.na(.))) %>% glimpse()

# exploratory data - uncomment if you want to see pivot table
#data %>% rpivotTable()

