# Navigating Renewable Energy Markets: A Utility Company's Guide to Effective Forecasting
This is Team 37's group project GitHub repository for MGT 6203 (Canvas) Fall of 2023 semester.

## Overview

This project aims to predict electric demand and pricing by analyzing weather patterns and energy generation options. Our goal is to assist utility companies in making informed decisions on renewable energy investments. We focus on Spain's energy market, leveraging datasets from ENTSOE and Spanish TSO Red Electric España, complemented by weather insights from the Open Weather API.

## Objectives

1. Predict future energy pricing and demand based on energy consumption patterns and weather.
2. Compare the performance of our predictive models against TSO forecasts.
3. Determine the impact of specific weather measurements on electrical consumption and pricing.
4. Optimize renewable energy operations based on predicted energy demands.

## Data

The datasets include:
- Electrical consumption, generation, and pricing data.
- Weather data such as temperature, humidity, wind speed, and precipitation.

## Methodology

- **Data Cleaning**: Standardizing column names, combining datasets, quality checks.
- **Load Prediction**: Random Forest model to predict electric load.
- **Price Prediction**: Linear Regression to forecast electricity pricing.
- **Model Evaluation**: Using metrics like RMSE, MSE, and MAPE for model comparison.

# Running the Code

## Project Directory Structure

- **Data**: Contains all the datasets used in the project.
- **Code**: Includes the R scripts and other code files for running the analysis.
- **Visualizations**: Stores generated charts, graphs, and other visual outputs.

## How to Run the Code

1. **Clone the Repository**: Use Git to clone the repository to your local machine.
2. **Navigate to Project Directory**: Change to the directory where the project is cloned.
3. **Package Management**: Run `renv::init()` in the R console. This will set up the project environment with the required packages. If this does not work, please refer to `Requirements.txt` for the list of packages used. 
4. **Executing Scripts**: Navigate to the 'Code' folder and open the R scripts included in the repository. Run them in RStudio in order of the numbering.
