library(randomForest)
library(dplyr)

ycol <- "total_load_actual"
xcols <- c("hour", "weekend", "month", "price_actual","temp","humidity",
           "wind_speed","rain_1h","rain_3h","snow_3h","clouds_all")
# Filter data for relevant columns then drop rows with missing values
df <- data %>% select(c(ycol,xcols))
df <- df %>% na.omit() 
# mtry: number of randomly sampled variables to select from at each split
# ntree: number of trees to grow
mtry <- sqrt(ncol(df)) 
ntree <- 500

set.seed(32) 

# Create a baseline model
rf_b <- randomForest(ycol ~ ., df)
rf_b

