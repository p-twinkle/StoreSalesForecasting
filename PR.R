######################################################################################################################
# Purpose: Sales forecasting for an organization(Not at store level)
# Author: Twinkle Panda
# Last Modified: 12th Dec 
######################################################################################################################

# Load required libraries
library(dplyr)
library(tidyr)
library(fable)
library(caret)
library(tsibble)
library(lubridate)

root_dir <- "/Users/twinklepanda/Desktop/Me/MSBA/4.2 Fall/SupplyChainAnalytics/Project"
data_dir <- file.path(root_dir, 'data')

setwd(root_dir)
# file.path(data_dir, )

# Read and prepare the data
train_data <- read.csv(file.path(data_dir, "train.csv"))
stores <- read.csv(file.path(data_dir, "stores.csv")) 
holidays <- read.csv(file.path(data_dir, "holidays_events.csv")) 
oil <- read.csv(file.path(data_dir, "oil.csv")) 
test_data <- read.csv(file.path(data_dir, "test.csv")) 
print('STEP 1: Data Imported')

# Convert the date columns to date format
print('STEP 2: Pre-Processing Start')
train_data <- train_data %>% mutate(date = as.Date(date)) 
test_data <- test_data %>% mutate(date = as.Date(date)) 
holidays <- holidays %>% mutate(date = as.Date(date)) 
oil <- oil %>% mutate(date = as.Date(date)) 
print('STEP 2.1: Date Column Typecast Complete')

# Drop duplicates
train_data   <- train_data[!duplicated(train_data), ]
test_data    <- test_data[!duplicated(test_data), ]
holidays     <- holidays[!duplicated(holidays), ]
oil          <- oil[!duplicated(oil), ]
stores       <- stores[!duplicated(stores), ]
print('STEP 2.2: Duplicates Dropped')

# Holiday flag: pre-processing
holiday <- holidays %>%
  filter(type %in% c('Holiday', 'Event')) %>%
  mutate(holiday_flag = ifelse(!is.na(locale), 1, 0)) %>% # Creating the flag: 1 if not null, 0 otherwise
  select(date, holiday_flag) %>%
  distinct()
print('STEP 2.3: Holiday Data Pre-processed')
#

# Categories: pre-processing: Reduces the number of categories from 33 to 7
category_groups <- c(
  AUTOMOTIVE = "NON-FOOD",
  `BABY CARE` = "PERSONAL CARE",
  BEAUTY = "PERSONAL CARE",
  BEVERAGES = "FOOD",
  BOOKS = "STATIONERY",
  `BREAD/BAKERY` = "FOOD",
  CELEBRATION = "NON-FOOD",
  CLEANING = "HOME CARE",
  DAIRY = "FOOD",
  DELI = "FOOD",
  EGGS = "FOOD",
  `FROZEN FOODS` = "FOOD",
  `GROCERY I` = "FOOD",
  `GROCERY II` = "FOOD",
  HARDWARE = "NON-FOOD",
  `HOME AND KITCHEN I` = "HOME CARE",
  `HOME AND KITCHEN II` = "HOME CARE",
  `HOME APPLIANCES` = "ELECTRONICS",
  `HOME CARE` = "HOME CARE",
  LADIESWEAR = "NON-FOOD",
  `LAWN AND GARDEN` = "NON-FOOD",
  LINGERIE = "NON-FOOD",
  `LIQUOR,WINE,BEER` = "ALCOHOL",
  MAGAZINES = "STATIONERY",
  MEATS = "FOOD",
  `PERSONAL CARE` = "PERSONAL CARE",
  `PET SUPPLIES` = "NON-FOOD",
  `PLAYERS AND ELECTRONICS` = "ELECTRONICS",
  POULTRY = "FOOD",
  `PREPARED FOODS` = "FOOD",
  PRODUCE = "FOOD",
  `SCHOOL AND OFFICE SUPPLIES` = "STATIONERY",
  SEAFOOD = "FOOD"
)

train_data <- train_data %>%
  mutate(family = category_groups[family])
test_data <- test_data %>%
  mutate(family = category_groups[family])
print('STEP 2.4: Family Column Pre-processed ')

## Since we grouped the category column, we need to aggregate the sales and onpromotion columns
# Remove ID column
train_data <- train_data %>% select(-id)
test_data  <- test_data %>% select(-id)

# Group by and aggregate
train_data <- train_data %>%
  group_by(date, store_nbr, family) %>%
  summarise(
    total_sales = sum(sales, na.rm = TRUE),
    total_onpromotion = sum(onpromotion, na.rm = TRUE)
  ) %>%
  ungroup()

test_data <- test_data %>%
  group_by(date, store_nbr, family) %>%
  summarise(
    total_onpromotion = sum(onpromotion, na.rm = TRUE)
  ) %>%
  ungroup()

print('STEP 2.5: Grouping & Aggregation Complete ')
print('STEP 2: Complete ')

##

# Create Final ADS

print('STEP 3: Final ADS Creation Start')
# Train
train_merged <- train_data %>%
  left_join(holiday, by = "date", suffix = c("", "_hol")) %>%
  left_join(oil, by = "date") %>%
  left_join(stores, by = "store_nbr", suffix = c("", "_str")) 
 # %>% left_join(transactions, c("date", "store_nbr")) 

# Test
test_merged <- test_data %>%
  left_join(holiday, by = "date", suffix = c("", "_hol")) %>%
  left_join(oil, by = "date") %>%
  left_join(stores, by = "store_nbr", suffix = c("", "_str")) 
 # %>% left_join(transactions, c("date", "store_nbr"))

print('STEP 3.1: Merged Datasets Created')

# For numerical columns: fill NA with 0 : NA were there in transactions and in oil prices
train_merged <- train_merged%>%
  mutate(across(where(is.numeric), ~replace_na(., 0)))
test_merged <- test_merged%>%
  mutate(across(where(is.numeric), ~replace_na(., 0)))
print('STEP 3.2: NULL Imputation - Complete')

sapply(train_merged, unique)
sapply(test_merged, unique)

write.csv(train_merged, "processed/train_merged.csv", row.names=FALSE)
write.csv(test_merged, "processed/test_merged.csv", row.names=FALSE)

# OHE for categorical columns

one_hot_encode <- function(df) {
  # Identify categorical columns
  categorical_cols <- sapply(df, function(x) is.factor(x) || is.character(x))
  
  # If there are no categorical columns, return the original df
  if (sum(categorical_cols) == 0) {
    return(df)
  }
  
  # Create dummyVars object
  dummy <- dummyVars(" ~ .", data = df[, categorical_cols, drop = FALSE])
  
  # Perform one-hot encoding
  encoded_df <- predict(dummy, newdata = df)
  
  # Combine encoded columns with non-categorical columns
  final_df <- cbind(df[, !categorical_cols, drop = FALSE], encoded_df)
  
  return(final_df)
}

# Function call
train_ads <- one_hot_encode(train_merged)
test_ads  <- one_hot_encode(test_merged)
print('STEP 3.3: OHE - Complete ')

write.csv(train_ads, "processed/train_ads.csv", row.names=FALSE)
write.csv(test_ads, "processed/test_ads.csv", row.names=FALSE)

print('STEP 3: Train & Test ADS Creation - Complete')

## 

########################## Forecasting #######################################################

# Load required libraries
library(fpp3)
library(urca)
library(tseries)

# Data Import
data <- read.csv("processed/train_merged.csv")
data <- data %>%
  mutate(date = as.Date(date, format = "%Y-%m-%d"))

################### V1: Day level forecast - ARIMA & ARIMA with 3 regressors ##################

# Load and prepare the data
store_sales <- data %>%
  group_by(date) %>%
  summarise(
    sales = sum(total_sales),
    onpromotion = sum(total_onpromotion),
    holiday_flag = max(holiday_flag),
    dcoilwtico = mean(dcoilwtico)
  ) %>%
  as_tsibble(index = date) %>%
  fill_gaps()

store_sales <- store_sales %>%
  mutate(across(c(sales, onpromotion), ~replace_na(., 0)))

# Visualize the total daily sales
store_sales %>%
  autoplot(sales) +
  labs(title = "Total Daily Store Sales", y = "Sales", x = "Date")

# Check stationarity and apply differencing if necessary
store_sales_diff <- store_sales %>%
  mutate(diff_sales = difference(sales))

# Plot ACF and PACF to assess stationarity
store_sales_diff %>%
  ACF(diff_sales, lag_max = 30) %>%
  autoplot() +
  labs(title = "ACF of Differenced Daily Sales")

store_sales_diff %>%
  PACF(diff_sales, lag_max = 30) %>%
  autoplot() +
  labs(title = "PACF of Differenced Daily Sales")

# Perform unit root tests
store_sales_diff %>%
  features(sales, unitroot_kpss)

store_sales_diff$sales %>% tseries::adf.test()

# Train - Test Split
train_sales <- store_sales %>% 
  filter(date <= max(date) - days(30))  # Use all but last 30 days for training

test_sales <- store_sales %>% 
  filter(date > max(date) - days(30))   # Use last 30 days for testing

# Fit the model on training data
fit <- train_sales %>%
  model(
    arima_auto = ARIMA(sales),
    arima_with_regressors = ARIMA(sales ~ onpromotion + holiday_flag + dcoilwtico)
  )

# Model report
fit %>% report()

# Perform residual diagnostics
fit %>% 
  augment() %>%
  features(.resid, ljung_box, lag = 10)

# Generate forecasts for the test period
forecasts <- fit %>%
  forecast(new_data = test_sales)

# Evaluate forecast accuracy
accuracy <- forecasts %>%
  accuracy(test_sales)

############################## V2 SARIMA #############################################################
# Based on the ACF and PACF plots shown, which display clear weekly seasonality (lag 7) 
# and significant spikes, we include seasonal components.

# Load and prepare the data with categorical variables
store_sales <- data %>%
  group_by(date) %>%
  summarise(
    sales = sum(total_sales),
    onpromotion = sum(total_onpromotion),
    holiday_flag = max(holiday_flag),
    dcoilwtico = mean(dcoilwtico)
  ) %>%
  as_tsibble(index = date) %>%
  fill_gaps()

# Split into train and test
train_sales <- store_sales %>% 
  filter(date <= max(date) - days(30))

test_sales <- store_sales %>% 
  filter(date > max(date) - days(30))


# Fit seasonal ARIMA model with regressors
fit_seasonal <- train_sales %>%
  model(
    sarima_reg = ARIMA(sales ~ onpromotion + holiday_flag + dcoilwtico + 1 + 
                         pdq(2,1,1) + PDQ(1,1,1,7))
  )


# Generate forecasts using test data
forecasts <- fit_seasonal %>%
  forecast(new_data = test_sales)

# Check accuracy
accuracy <- forecasts %>%
  accuracy(test_sales)

############################# V3: Category-wise Forecast #########################################
# Prepare data for FOOD category
store_sales <- data %>%
  filter(family == "FOOD")  %>%
  group_by(date) %>%
  summarise(
    sales = sum(total_sales),
    onpromotion = sum(total_onpromotion),
    holiday_flag = max(holiday_flag),
    dcoilwtico = mean(dcoilwtico)
  ) %>%
  as_tsibble(index = date) %>%
  fill_gaps()

# Split into train and test
train_sales <- store_sales %>% 
  filter(date <= max(date) - days(30))

test_sales <- store_sales %>% 
  filter(date > max(date) - days(30))

fit_food <- train_sales %>%
  model(
    sarima_food = ARIMA(sales ~ onpromotion + holiday_flag + dcoilwtico + 1 + 
                          pdq(6,1,1) + PDQ(1,1,1,7))
  )

fit <- train_sales %>%
  model(
    arima_auto = ARIMA(sales),
    arima_with_regressors = ARIMA(sales ~ onpromotion + holiday_flag + dcoilwtico)
  )

# Generate forecasts
food_forecasts <- fit_food %>%
  forecast(new_data = test_sales)

# Check accuracy
accuracy <- forecasts %>%
  accuracy(test_sales)

############################# V4: StoreCluster wise forecast #########################################
# Prepare data for Store Cluster
store_sales <- data %>%
  filter(cluster == 13)  %>%
  group_by(date) %>%
  summarise(
    sales = sum(total_sales),
    onpromotion = sum(total_onpromotion),
    holiday_flag = max(holiday_flag),
    dcoilwtico = mean(dcoilwtico)
  ) %>%
  as_tsibble(index = date) %>%
  fill_gaps()

# Split into train and test
train_sales <- store_sales %>% 
  filter(date <= max(date) - days(30))

test_sales <- store_sales %>% 
  filter(date > max(date) - days(30))

# SARIMA
fit_sarima_sc <- train_sales %>%
  model(
    sarima_sc = ARIMA(sales ~ onpromotion + holiday_flag + dcoilwtico + 1 +
                          pdq(6,1,1) + PDQ(1,1,1,7))
  )

# Generate forecasts
forecasts_sarima_sc <- fit_sarima_sc %>%
  forecast(new_data = test_sales)

# Check accuracy
accuracy_sarima_sc <- forecasts_sarima_sc %>%
  accuracy(test_sales)

# ARIMA
fit_arima_sc <- train_sales %>%
  model(
    arima_auto = ARIMA(sales),
    arima_with_regressors = ARIMA(sales ~ onpromotion + holiday_flag + dcoilwtico)
  )

# Generate forecasts
forecasts_arima_sc <- fit_arima_sc %>%
  forecast(new_data = test_sales)

# Check accuracy
accuracy_arima_sc <- forecasts_arima_sc %>%
  accuracy(test_sales)

######################################### Lagged promotions #########################################
# Prepare data with lagged promotions
store_sales_lagged <- store_sales %>%
  mutate(
    promo_lag1 = lag(onpromotion, 1),
    promo_lag2 = lag(onpromotion, 2),
    promo_lag3 = lag(onpromotion, 3),
    promo_lag7 = lag(onpromotion, 7),  # Weekly lag based on ACF
    promo_lag14 = lag(onpromotion, 14)  # Bi-weekly lag based on ACF
  ) %>%
  filter(!is.na(promo_lag14))  # Remove NAs from lagged values

# Split into train and test
train_sales <- store_sales_lagged %>% 
  filter(date <= max(date) - days(30))

test_sales <- store_sales_lagged %>% 
  filter(date > max(date) - days(30))

# Fit SARIMA model with lagged promotions
fit_lagged <- train_sales %>%
  model(
    sarima_promo = ARIMA(sales ~ onpromotion + promo_lag1 + promo_lag2 + 
                           promo_lag3 + promo_lag7 + promo_lag14 + 
                           holiday_flag + dcoilwtico + 1 + 
                           pdq(2,1,1) + PDQ(1,1,1,7))
  )

# Generate forecasts
forecasts <- fit_lagged %>%
  forecast(new_data = test_sales)

# Check accuracy
accuracy <- forecasts %>%
  accuracy(test_sales)

########################## Naive Forecast: Baseline For Comparison ####################################

# Load and prepare the data
store_sales_naive <- data %>%
  group_by(date) %>%
  summarise(
    sales = sum(total_sales),
    onpromotion = sum(total_onpromotion),
    holiday_flag = max(holiday_flag),
    dcoilwtico = mean(dcoilwtico)
  ) %>%
  as_tsibble(index = date) %>%
  fill_gaps()

# Split into train and test
train_sales <- store_sales_naive %>% 
  filter(date <= max(date) - days(30))

test_sales <- store_sales_naive %>% 
  filter(date > max(date) - days(30))

# Naive forecast (using previous day)
naive_forecast <- train_sales %>%
  model(
    NAIVE(sales)
  )

# Generate forecasts for test period
forecasts <- naive_forecast %>%
  forecast(new_data = test_sales)

# Check accuracy
accuracy_naive <- forecasts %>%
  accuracy(test_sales)


####################### END ###########################################################




