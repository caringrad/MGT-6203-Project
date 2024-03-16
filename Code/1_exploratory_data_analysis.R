library(ggplot2)
library(dplyr)
library(tidyr)
library(corrplot)

# energy demand over time

average_hourly_demand_by_season <- data %>%
  group_by(hour, season) %>%
  summarise(average_demand = mean(total_load_actual, na.rm = TRUE))

ggplot(average_hourly_demand_by_season,
       aes(x = hour, y = average_demand, group = season, color = season)) +
  geom_line() +
  facet_wrap(~season, scales = "free_y") +
  theme_minimal() +
  labs(title = "Average Hourly Energy Demand for Each Season",
       x = "Hour of Day",
       y = "Average Energy Demand (MWh)")


# energy price over time

average_hourly_price_by_season <- data %>%
  group_by(hour, season) %>%
  summarise(average_price = mean(price_actual, na.rm = TRUE))

ggplot(average_hourly_price_by_season,
       aes(x = hour, y = average_price, group = season, color = season)) +
  geom_line() +
  facet_wrap(~season, scales = "free_y") +
  theme_minimal() +
  labs(title = "Average Hourly Price for Each Season",
       x = "Hour of Day",
       y = "Average Price (EUR/MWh)")


# actual vs. forecasted load comparison

average_load_by_season <- data %>%
  group_by(hour, season) %>%
  summarise(average_forecasted_load = mean(total_load_forecast, na.rm = TRUE),
            average_actual_load = mean(total_load_actual, na.rm = TRUE))

average_load_long <- average_load_by_season %>%
  pivot_longer(cols = c(average_forecasted_load, average_actual_load), 
               names_to = "type", 
               values_to = "average_load")

ggplot(average_load_long, aes(x = hour, y = average_load, group = type, color = type)) +
  geom_line() +
  facet_wrap(~season, scales = "free_y") +
  theme_minimal() +
  labs(title = "Comparison of Forecasted vs. Actual Energy Load (24-Hour Average) Per Season",
       x = "Hour of Day",
       y = "Average Load (MWh)")


# energy generation bar chart

data_energy_added_cols <- data %>% 
  mutate(
    generation_wind = generation_wind_offshore + generation_wind_onshore,
    generation_oil = generation_fossil_oil + generation_fossil_oil_shale,
    generation_gas = generation_fossil_coal_derived_gas + generation_fossil_gas,
    generation_coal = generation_fossil_brown_coal_lignite + generation_fossil_hard_coal,
    generation_hydro = generation_hydro_pumped_storage_consumption + generation_hydro_run_of_river_and_poundage + generation_hydro_water_reservoir,
    generation_other = generation_other + generation_other_renewable + generation_waste + generation_biomass + generation_fossil_peat
)

generation_columns <- c(
  "generation_wind",
  "generation_nuclear",
  "generation_oil",
  "generation_gas",
  "generation_coal",
  "generation_hydro",
  "generation_solar",
  "generation_other"
  )

average_generation_by_season <- data_energy_added_cols %>%
  select(hour, season, all_of(generation_columns)) %>%
  group_by(hour, season) %>%
  summarise_all(mean, na.rm = TRUE)

average_generation_long <- average_generation_by_season %>%
  pivot_longer(cols = generation_columns, names_to = "generation_source", values_to = "average_generation")

ggplot(average_generation_long,
       aes(x = generation_source, y = average_generation, fill = generation_source)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~season, ncol = 1, scales = "free_y") +  
  theme_minimal() +
  theme(
    strip.text.x = element_text(size = 12, margin = margin(1, 0, 1, 0)),
    strip.background = element_rect(colour = "white", fill = "white"), 
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.margin = unit(c(1, 1, 1, 1), "lines"), 
    panel.spacing = unit(2, "lines")
  ) +
  labs(title = "Average 24-Hour Energy Generation by Source and Season",
       x = "Energy Source",
       y = "Average Generation (MWh)")


# correlation heatmap

relevant_columns <- c("temp", "pressure", "humidity", "wind_speed",  
                      "total_load_actual", "price_actual", "weekend")

correlation_data <- data %>%
  select(all_of(relevant_columns)) %>%
  na.omit() 

correlation_matrix <- cor(correlation_data)
corrplot(correlation_matrix, method = "color", type = "upper", 
         tl.col = "black", tl.srt = 45, 
         title = "Correlation Heatmap")

