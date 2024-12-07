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
transactions <- read.csv(file.path(data_dir, "transactions.csv")) 
test_data <- read.csv(file.path(data_dir, "test.csv")) 
print('STEP 1: Data Imported')

# Convert the date columns to date format
print('STEP 2: Pre-Processing Start')
train_data <- train_data %>% mutate(date = as.Date(date)) 
test_data <- test_data %>% mutate(date = as.Date(date)) 
holidays <- holidays %>% mutate(date = as.Date(date)) 
oil <- oil %>% mutate(date = as.Date(date)) 
transactions <- transactions %>% mutate(date = as.Date(date)) 
print('STEP 2.1: Date Column Typecast Complete')

# Drop duplicates
train_data   <- train_data[!duplicated(train_data), ]
test_data    <- test_data[!duplicated(test_data), ]
holidays     <- holidays[!duplicated(holidays), ]
oil          <- oil[!duplicated(oil), ]
transactions <- transactions[!duplicated(transactions), ]
stores       <- stores[!duplicated(stores), ]
print('STEP 2.2: Duplicates Dropped')

# Holiday flag: pre-processing
holiday <- holidays %>%
  filter(type %in% c('Holiday', 'Event')) %>%
  select(date, locale) %>%
  mutate(locale = ifelse(locale == "Regional", "Local", locale)) %>%
  distinct()

# For dates that are mapped to both categories: 'Local' and 'National', we'll retain 'National' record, PK: Date
holiday <- holiday %>%
  group_by(date) %>%
  filter(!(locale == "Local" & any(locale == "National"))) %>%
  ungroup()
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
print('STEP 2: Complete ')
##

# Create Final ADS

print('STEP 3: Final ADS Creation Start')
# Train
train_merged <- train_data %>%
  left_join(holiday, by = "date", suffix = c("", "_hol")) %>%
  left_join(oil, by = "date") %>%
  left_join(stores, by = "store_nbr", suffix = c("", "_str")) %>%
  left_join(transactions, c("date", "store_nbr")) 

# Test
test_merged <- test_data %>%
  left_join(holiday, by = "date", suffix = c("", "_hol")) %>%
  left_join(oil, by = "date") %>%
  left_join(stores, by = "store_nbr", suffix = c("", "_str")) %>%
  left_join(transactions, c("date", "store_nbr"))

print('STEP 3.1: Merged Datasets Created')

# For numerical columns: fill NA with 0 : NA were there in transactions and in oil prices
train_merged <- train_merged%>%
  mutate(across(where(is.numeric), ~replace_na(., 0)))
test_merged <- test_merged%>%
  mutate(across(where(is.numeric), ~replace_na(., 0)))
print('STEP 3.2: NULL Imputation - Complete')

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
print('STEP 3: Train & Test ADS Creation - Complete')

## 


''' 
Next steps:
- In test_ads there is a categorical column that has less categories than in train - Handle that
- We have a city column with lots of categories - Need to see if they can be grouped together.
  The city column could be the reason for uneven cats
- Time Series Forecasting - TBD
    - Train Val split
    - Predict for test_ads
    - Evaluate
'''



########################## Forecasting #################################################
# Code below this is not verified and has to be tweaked

# Convert to tsibble format
train_tsibble <- train_data %>%
  mutate(date = as.Date(date)) %>%
  left_join(stores, by = "store_nbr") %>%
  as_tsibble(key = c(store_nbr, family), index = date)

# Create a model for each store and product family combination
arima_model <- train_tsibble %>%
  model(
    arima = ARIMA(sales ~ onpromotion + cluster + PDQ(0,0,0))
  )

# Generate forecasts for the next 15 days
forecast_horizon <- 15
forecasts <- arima_model %>%
  forecast(h = forecast_horizon)

# View the forecasts
forecasts %>%
  filter(store_nbr == 1, family == "GROCERY I") %>%
  autoplot()

# Evaluate the model
accuracy(arima_model)