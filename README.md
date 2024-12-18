# StoreSalesForecasting

## Overview

This project aims to develop an accurate and reliable forecasting model to predict daily sales volume for Favorita stores and their various product families in Ecuador. Using advanced time-series forecasting techniques, including ARIMA and Bayesian modeling, we analyze historical sales data, promotional information, and external factors to create a comprehensive forecasting solution.

## Dataset

The dataset used in this project is from the Kaggle competition "Store Sales - Time Series Forecasting." It includes:

- Historical sales data (2013-2017)
- Store information (location, type, cluster)
- Product family details
- Promotional data
- Holiday and event information
- Daily oil prices

## Features

- Time-series analysis of sales trends
- Integration of external factors (holidays, oil prices, promotions)
- Store and product family level forecasting
- Seasonal and non-seasonal ARIMA modeling
- Exploratory Data Analysis (EDA) of sales patterns

## Key Findings

- Champion model: SARIMA with regressors at store-cluster level (MAPE: 6.51%, MAE: 1.58)
- Challenger model: Base ARIMA at day-level (MAPE: 7.18%, MAE: -2.39)
- Both models significantly outperformed the naive baseline forecast

## Technologies Used

- R (for ARIMA modeling)
- Stan (for Bayesian modeling attempts)
- Various data preprocessing and visualization libraries

## Future Scope

- Incorporate additional engineered features
- Experiment with hierarchical forecasting
- Develop hybrid and ensemble approaches
- Explore more advanced machine learning techniques

## Contributors

- Ashley Hattendorf
- Brooks Li
- Brinda Asuri
- Sarah Dominguez
- Twinkle Panda

## Acknowledgments
This project was completed as part of the Analytics for Supply Chain and Distribution Plan (OM 286) course at the University of Texas at Austin, under the guidance of Dr. Genaro Gutierrez.

