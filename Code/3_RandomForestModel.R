library(randomForest)
library(dplyr)

ycol <- "total_load_actual"
xcols <- c("hour", "weekend", "season", "price_actual","temp","humidity",
           "wind_speed","rain_1h","rain_3h","snow_3h","clouds_all", "year")
# Filter data for relevant columns then drop rows with missing values
 
df <- data %>% select(c(ycol,xcols)) # Filter df for Jan 2015 - Dec 2015
# test <- filter for Jan - Feb 2016

df <- df %>% na.omit() 

testdf <- df[df$year == 2016,]
traindf <- df[df$year == 2015,]

df <- subset(df, select = -year)
testdf <- subset(testdf, select = -year)
traindf <- subset(traindf, select = -year)

xcols <- c("hour", "weekend", "season", "price_actual","temp","humidity",
           "wind_speed","rain_1h","rain_3h","snow_3h","clouds_all")
  
# mtry: number of randomly sampled variables to select from at each split
# ntree: number of trees to grow
mtry <- sqrt(ncol(df)) 
ntree <- 500

set.seed(32) 

# Create a baseline model
#rf_b <- randomForest(total_load_actual ~ ., df, mtry = mtry, ntree=ntree)

# Use only 2015 data (training) for random forest model
rf_b <- randomForest(total_load_actual ~ ., traindf, mtry = mtry, ntree=ntree)
rf_b

## Plot MSE as a function of number of trees
plot(rf_b)
# 500 trees is far too much -- Error levels off around 200
ntree_best <- 200
## Plot variable importance - #%Inc MSE less biased than IncNodePurity
varImpPlot(rf_b, 
           sort=TRUE, 
           main="Load Prediction Variable Importance")
## Filter for important variables
## Tune mtry with tuneRF(). 
## Do we also want to tune nodesize?? ## ONLY IF WE DON'T HAVE "GOOD" model
rf_tuned <- tuneRF(
  x=traindf[xcols], #define predictor variables
  y=traindf$total_load_actual, #define response variable
  ntreeTry=ntree_best,
  mtryStart=mtry, 
  stepFactor=1.5,
  improve=0.01,
  trace=FALSE #don't show real-time progress
)

rf_tuned_model <- randomForest(total_load_actual ~ ., traindf, mtry = 3, ntree=200)


#Predictions using baseline model
predictions <- predict(rf_b, newdata = testdf)
testdf$preds <- predictions
testdf$pe <- abs(testdf$total_load_actual - testdf$preds)/abs(testdf$total_load_actual)
## COMPUTE ACCURACY
MSE <- mean((testdf$total_load_actual - predictions)^2)
RMSE <- sqrt(mean((testdf$total_load_actual - predictions)^2))
MAPE <- mean(testdf$pe)


#Predictions using tuned model
predictions <- predict(rf_tuned_model, newdata = testdf)
testdf$preds <- predictions
testdf$pe <- abs(testdf$total_load_actual - testdf$preds)/abs(testdf$total_load_actual)
## COMPUTE ACCURACY
MSE <- mean((testdf$total_load_actual - predictions)^2)
RMSE <- sqrt(mean((testdf$total_load_actual - predictions)^2))
MAPE <- mean(testdf$pe)


set.seed(32)
library(caret)
ctrl <- trainControl(method = "cv", number = 5)
rf_model_caret <- train(total_load_actual ~ ., data = traindf, method = "rf", trControl = ctrl)
varImp(rf_model_caret)
#Predictions using tuned model
predictions <- predict(rf_model_caret, newdata = testdf)
testdf$preds <- predictions
testdf$pe <- abs(testdf$total_load_actual - testdf$preds)/abs(testdf$total_load_actual)
## COMPUTE ACCURACY
MSE <- mean((testdf$total_load_actual - predictions)^2)
RMSE <- sqrt(mean((testdf$total_load_actual - predictions)^2))
MAPE <- mean(testdf$pe)





ctrl <- rfeControl(functions = rfFuncs, method = "cv", number = 5)
method_params <- list(mtry = 3, ntree = 200) # Set your desired mtry and ntree values
ctrl$methodParams <- method_params

rf_rfe <- rfe(df[, -which(names(df) == "total_load_actual")], df$total_load_actual, 
              sizes = 1:ncol(df), 
              rfeControl = ctrl, 
              method = "rf")

selected_features <- rf_rfe$optVariables
selected_features <- append(selected_features,'total_load_actual')
subtestdf <- subset(testdf, select = selected_features)
subtraindf <- subset(traindf, select = selected_features)

rf_submodel <- randomForest(total_load_actual ~ ., data = subtraindf, mtry=3, ntree=200)

subpredictions <- predict(rf_submodel, newdata = subtestdf)
subtestdf$preds <- subpredictions
subtestdf$pe <- abs(subtestdf$total_load_actual - subtestdf$preds)/abs(subtestdf$total_load_actual)

subMSE <- mean((subtestdf$total_load_actual - subpredictions)^2)
subRMSE <- sqrt(mean((subtestdf$total_load_actual - subpredictions)^2))
subMAPE <- mean(subtestdf$pe)