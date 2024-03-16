### Regression Script
set.seed(6203)

library(regclass)
library(Metrics)
library(dplyr)
library(glmnet)
library(ggplot2)
library(caret)
library(MASS)
library(car)



## Test/Train Set Split - Ty
  ## 80/20 split

  totalRows = 1:nrow(data)
  trainRows = sample(totalRows,size = round((nrow(data) * 0.8),0),replace = F)
  regTrainData = data.frame(data[trainRows,])
  regTestData = data.frame(data[-trainRows,])
  
  #converting string time to datetime
  regTrainData$time = as.POSIXct(regTrainData$time, format="%Y-%m-%d %H:%M:%S", tz="UTC")
  regTestData$time = as.POSIXct(regTestData$time, format="%Y-%m-%d %H:%M:%S", tz="UTC")
  
  #find all numeric cols with all 0's (22,26,27,28,32,38)
  all0s = which(colSums(data[,-c(1,6,7,8)],na.rm = T) == 0)+4
  
  #taking out city name and sum 0 cols
  regTrainData = regTrainData[,-c(8,all0s)]
  regTestData = regTestData[,-c(8,all0s)]

## Exploratory Data Analysis
  
  # distribution among years for test / train data - no certain year is over/under represented
  # training/testing data year distribution in %
  round(table(regTrainData$year)/nrow(data) * 100,2)
  round(table(regTestData$year)/nrow(data) * 100,2)
  
  # correlation between predictors and response
  cor(data[,-c(1,6,7,8,all0s,45)],data[,45])
  
  View(regTrainData)
  

## Models
  
  # Full Model - Ty
    #fit Model
    fullMod = lm(price_actual~.,data = regTrainData)
    summary(fullMod)
    #find any outliers and remove them
    fullCook = cooks.distance(fullMod)
    fullOutliers = which(fullCook >= (4/nrow(regTrainData)))
    regTrainDataNoOutliers = regTrainData[-fullOutliers,]
    #refit model with data that has outliers removed
    fullMod2 = lm(price_actual~.,data = regTrainDataNoOutliers)
    summary(fullMod2)
    #look for multicollinearity
    which(VIF(fullMod2)[,1] >= 10)
    #use box cox to check for optimal transformation
    fullmodBoxCox = boxcox(fullMod2)
    fullOptLamb = fullmodBoxCox$x[which(fullmodBoxCox$y==max(fullmodBoxCox$y))]
    #refit model without outliers and with transformed response var (https://www.statology.org/box-cox-transformation-in-r/)
    fullMod3 = lm(((price_actual^fullOptLamb-1)/fullOptLamb)~.,data = regTrainDataNoOutliers)
    summary(fullMod3)
    #get Predictions for full Mod1, full Mod2, and full Mod3
    fullModPred1 = round(predict(fullMod,regTestData,type="response"),2)
    fullModPred2 = round(predict(fullMod2,regTestData,type="response"),2)
    fullModPred3 = round(predict(fullMod3,regTestData,type="response"),2)

    #residual analysis - full
    fullModRes = rstandard(fullMod)
    plot(fullMod$fitted.values,fullModRes,xlab="fitted values",ylab="residuals",main="Full Model: Fitted vs Residuals")
    abline(h=0,col="red")
    qqnorm(fullModRes,main = "Full Model QQ Plot")
    qqline(fullModRes,col="red")
    hist(fullModRes,main = "Full Model Histogram")
    
    #residual analysis - full with outliers removed
    fullModRes2 = rstandard(fullMod2)
    plot(fullMod2$fitted.values,fullModRes2,xlab="fitted values",ylab="residuals",main="Full Model (no outliers): Fitted vs Residuals")
    abline(h=0,col="red")
    qqnorm(fullModRes2,main = "Full Model (no outliers) QQ Plot")
    qqline(fullModRes2,col="red")
    hist(fullModRes2,main = "Full Model (no outliers) Histogram")
    
    #residual analysis - full with outliers removed and response transformed Y^optlamb
    fullModRes3 = rstandard(fullMod3)
    plot(fullMod3$fitted.values,fullModRes3,xlab="fitted values",ylab="residuals",main="Full Model (no outliers & transformed): Fitted vs Residuals")
    abline(h=0,col="red")
    qqnorm(fullModRes3,main = "Full Model (no outliers & transformed) QQ Plot")
    qqline(fullModRes3,col="red")
    hist(fullModRes3,main = "Full Model (no outliers & transformed) Histogram")
    
    #get initial RMSE for full Mod1, full Mod2, and full Mod3
    fullMod1RMSE = rmse(regTestData$price_actual,fullModPred1)
    fullMod2RMSE = rmse(regTestData$price_actual,fullModPred2)
    fullMod3RMSE = rmse(regTestData$price_actual,fullModPred3)
    
    fullModelRMSE_Results = data.frame("Model" = c("Full Model","Full Model-No Outliers","Full Model-No Outliers-Transformed"),
                                       "RMSE" = c(fullMod1RMSE,fullMod2RMSE,fullMod3RMSE))
    
    #cross validate all 3 versions of full model
    set.seed(6203)
    b = 100
    fullModCVRMSE = NULL
    for (B in 1:b){
      temp_totalRows = 1:nrow(data)
      temp_trainRows = sample(temp_totalRows,size = round((nrow(data) * 0.8),0),replace = F)
      temp_regTrainData = data.frame(data[temp_trainRows,])
      temp_regTestData = data.frame(data[-temp_trainRows,])
      #converting string time to datetime
      temp_regTrainData$time = as.POSIXct(temp_regTrainData$time, format="%Y-%m-%d %H:%M:%S", tz="UTC")
      temp_regTestData$time = as.POSIXct(temp_regTestData$time, format="%Y-%m-%d %H:%M:%S", tz="UTC")
      #find all numeric cols with all 0's (22,26,27,28,32,38)
      temp_all0s = which(colSums(data[,-c(1,6,7,8)],na.rm = T) == 0)+4
      #taking out city name and sum 0 cols
      temp_regTrainData = temp_regTrainData[,-c(8,temp_all0s)]
      temp_regTestData = temp_regTestData[,-c(8,temp_all0s)]
      
      #fit Model
      temp_fullMod = lm(price_actual~.,data = temp_regTrainData)
      #find any outliers and remove them
      temp_fullCook = cooks.distance(temp_fullMod)
      temp_fullOutliers = which(temp_fullCook >= (4/nrow(temp_regTrainData)))
      temp_regTrainDataNoOutliers = temp_regTrainData[-temp_fullOutliers,]
      #refit model with data that has outliers removed
      temp_fullMod2 = lm(price_actual~.,data = temp_regTrainDataNoOutliers)
      #use box cox to check for optimal transformation
      temp_fullmodBoxCox = boxcox(temp_fullMod2)
      temp_fullOptLamb = temp_fullmodBoxCox$x[which(temp_fullmodBoxCox$y==max(temp_fullmodBoxCox$y))]
      #refit model without outliers and with transformed response var (https://www.statology.org/box-cox-transformation-in-r/)
      temp_fullMod3 = lm(((price_actual^temp_fullOptLamb-1)/temp_fullOptLamb)~.,data = temp_regTrainDataNoOutliers)
      #get Predictions for full Mod1, full Mod2, and full Mod3
      temp_fullModPred1 = round(predict(temp_fullMod,temp_regTestData,type="response"),2)
      temp_fullModPred2 = round(predict(temp_fullMod2,temp_regTestData,type="response"),2)
      temp_fullModPred3 = round(predict(temp_fullMod3,temp_regTestData,type="response"),2)
      #get temp RMSE for full Mod1, full Mod2, and full Mod3
      temp_fullMod1RMSE = rmse(temp_regTestData$price_actual,temp_fullModPred1)
      temp_fullMod2RMSE = rmse(temp_regTestData$price_actual,temp_fullModPred2)
      temp_fullMod3RMSE = rmse(temp_regTestData$price_actual,temp_fullModPred3)
      
      fullModCVRMSE = rbind(fullModCVRMSE,cbind(temp_fullMod1RMSE,temp_fullMod2RMSE,temp_fullMod3RMSE))
      
    }
      colnames(fullModCVRMSE) = c("Full Model RMSE","Full Model-No Outliers RMSE","Full Model-No Outliers-Transformed")
      fullModCVRMSE
      fullModCVRMSE = apply(fullModCVRMSE,2,mean)
      
      #even after cross validation, the full models with no transformation and outliers left IN yeilds best average RMSE
      fullModCVRMSE
  
  # Stepwise Regression - Micah
    n = nrow(regTrainData)
    p = ncol(regTrainData)
      
    AIC(fullMod, k=2) # checking AIC
    AIC(fullMod,k=log(n)) #checking BIC
    
    min_model <- lm(price_actual ~ 1, regTrainData)
    step_mod <- step(min_model, scope = list(lower=min_model, upper=fullMod),direction = "forward", k=log(n),trace=F)
    
    
    summary(step_mod)
  
  # LASSO - Micah
    X = matrix(rnorm(n*p), ncol=p) #calculating x variable
    X = scale(X)
    colMeans(X) #mean
    apply(X,2,sd) #standard deviation
    
    x_vars <- model.matrix(price_actual~. , regTrainData)[,-38]
    y_var <- regTrainData$price_actual
    lambda_seq <- 10^seq(2, -2, by = -.1)
    
    lasso_mod <- cv.glmnet(x_vars, y_var,
                           alpha = 1, 
                           nfolds = 5)
    
    opt_lam <- lasso_mod$lambda.min #finding optimal lambda
    opt_lam
    
    lasso_mod2 <- glmnet(x_vars, y_var, alpha = 1, lambda = opt_lam) # Rebuilding model with optimal lamda
    
    coef(lasso_mod2) #shows that temp and pressure have been shrunk to 0
  
  # Elastic Net - Micah

    elastic_mod <- cv.glmnet(x_vars, y_var,
                           alpha = .5, 
                           nfolds = 5)
    
    opt_lam2 <- elastic_mod$lambda.min #finding optimal lambda
    opt_lam2
    
    elastic_mod2 <- glmnet(x_vars, y_var, alpha = .5, lambda = opt_lam) # Rebuilding model with optimal lamda
    
    coef(elastic_mod2) #shows that temp and pressure have been shrunk to 0
    
  # Creating model with stepwise-selected variables
    
    step_vars_mod <- lm(price_actual~clouds_all + forecast_solar_day_ahead + forecast_wind_onshore_day_ahead + generation_biomass + generation_fossil_brown_coal_lignite + generation_fossil_hard_coal + generation_hydro_pumped_storage_consumption + generation_hydro_run_of_river_and_poundage + generation_hydro_water_reservoir + generation_nuclear + generation_other + generation_other_renewable + generation_waste + hour + humidity + month + price_day_ahead + season + snow_3h + temp + temp_min + time + total_load_forecast + weekend + wind_deg + wind_speed
                            ,data = regTrainData)
    summary(step_vars_mod)
    
    #Residual analysis for stepwise model
    
          resstepwise = residuals(step_vars_mod)
          
          #constructing histogram to test normality
          hist(resstepwise, main="Histogram Examining Normality", nclass=20)
          
          #constructing a qqplot to test variance issues seen earlier
          qqPlot(resstepwise, ylab="residuals", main = "Testing Normality & Variance")
          
          #constructing a plot to test variance and heteroscedasticity
          plot(predict(step_vars_mod), resstepwise, xlab="Fitted Values",ylab="Residuals",main="Residuals vs Fitted Values")
          abline(0,0,col="red")
          lines(lowess(step_vars_mod$fitted.values,resstepwise), col="orange")
    
    #Creating model with lasso-selected variables
    lasso_vars_mod <- lm(price_actual ~. - pressure - temp,data = regTrainData)
    summary(lasso_vars_mod)
    
    
    #Residual analysis for lasso_vars_mod
    
          reslasso = residuals(lasso_vars_mod)
          
          #constructing histogram to test normality
          hist(reslasso, main="Histogram Examining Normality", nclass=20)
          
          #constructing a qqplot to test variance issues seen earlier
          qqPlot(reslasso, ylab="residuals", main = "Testing Normality & Variance")
          
          #constructing a plot to test variance and heteroscedasticity
          plot(predict(lasso_vars_mod), reslasso, xlab="Fitted Values",ylab="Residuals",main="Residuals vs Fitted Values")
          abline(0,0,col="red")
          lines(lowess(lasso_vars_mod$fitted.values,reslasso), col="orange")
    
    #Creating model with elastic net-selected variables
    elastic_vars_mod <- lm(price_actual ~. - temp,data = regTrainData)
    summary(elastic_vars_mod)
    
    #residual analysis for elastic_vars_mod
    
        reselastic = residuals(elastic_vars_mod)
        
        #constructing histogram to test normality
        hist(reselastic, main="Histogram Examining Normality", nclass=20)
        
        #constructing a qqplot to test variance issues seen earlier
        qqPlot(reselastic, ylab="residuals", main = "Testing Normality & Variance")
        
        #constructing a plot to test variance and heteroscedasticity
        plot(predict(elastic_vars_mod), reselastic, xlab="Fitted Values",ylab="Residuals",main="Residuals vs Fitted Values")
        abline(0,0,col="red")
        lines(lowess(elastic_vars_mod$fitted.values,reselastic), col="orange")
        
    #final RMSE on models
        #step
        stepPred = round(predict(step_vars_mod,regTestData,type="response"),2)
        stepRMSE = rmse(regTestData$price_actual,stepPred)        
        #lasso
        lassoPred = round(predict(lasso_vars_mod,regTestData,type="response"),2)
        lassoRMSE = rmse(regTestData$price_actual,lassoPred)        
        #elastic net
        enetPred = round(predict(elastic_vars_mod,regTestData,type="response"),2)
        enetRMSE = rmse(regTestData$price_actual,enetPred)
        
    rmseDF = data.frame("Model" = c("Full","Stepwise","LASSO","ElasticNet"),
                        "RMSE" = c(fullMod1RMSE,stepRMSE,lassoRMSE,enetRMSE))
    rmseDF[which.min(rmseDF$RMSE),]
    #LASSO has the best RMSE, but they are all pretty close
    
    #using anova between the full model and all models
    anova(fullMod, step_vars_mod)
    anova(fullMod, lasso_vars_mod)
    anova(fullMod, elastic_vars_mod)
    
    #Stepwise has the best adjusted R-squared in the anova