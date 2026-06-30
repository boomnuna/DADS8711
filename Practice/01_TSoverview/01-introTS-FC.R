#########################
# tsibble
#########################
library(fpp3)
library(tidyverse)

y_test_1 <- tsibble(
  Year = 2015:2026,
  Demand = c(190, 260, 330, 480, 570, 710, 650, 690, 730, 680, 660, 590),
  index = Year
)

y_test_2 <- tsibble(
  Year = yearquarter("2015 Q1") + 0:11,
  Demand = c(190, 260, 330, 480, 570, 710, 650, 690, 730, 680, 660, 590),
  index = Year
)

#########################
# How to read the external file and change to time-sereies table
#########################
library(readr)
library(dplyr)
library(tsibble)

tute1 <- read_csv("tute1.csv")
df <- tute1 %>% 
  mutate(Quarter = yearquarter(Quarter)) %>% 
  as_tsibble(index = Quarter)


##############################
# Hold-Out Spliting 
###############################
df <- read_csv("smallExample.csv")
glimpse(df)

df1 <- df %>%
  mutate(Quarter = yearquarter(Date)) %>% 
  as_tsibble(key = c(StoreID, SKU), index = Quarter) %>% 
  select(-Date)

# Create training set
train <- df1 %>% 
  filter(year(Quarter) <= 2002)

# Fit model on training set
fit <- train %>% 
  model(
    Mean = MEAN(Demand), 
    Naive = NAIVE(Demand),
    SeasonNaive = SNAIVE(Demand),
    Drift = NAIVE(Demand ~ drift())
)

accuracy(fit)  # Residual on train

# Forecast on test set
fc <- fit %>% 
  forecast(h=4)

# Point Forecats
accuracy(fc, df1)  # Error on test
# CRPS 
accuracy(fc, df1, measures = list(crps = CRPS)) %>% 
arrange(crps)

#########################
# Cross validation
#########################
train_CV <- train %>%
  filter(StoreID == 2, SKU == "A") %>% 
  slice(1:(n()-1)) %>% 
  stretch_tsibble(.init=3, .step=1)

fit <- train_CV %>% 
  model(
    Mean = MEAN(Demand), 
    Naive = NAIVE(Demand),
    SeasonNaive = SNAIVE(Demand),
    Drift = NAIVE(Demand ~ drift())
)

# Forecast on test set
fc <- fit %>% 
  forecast(h=4)
accuracy(fc, df1)  # Error on test
accuracy(fc, df1, measures = list(crps = CRPS)) %>% 
arrange(crps)

# Other servuce level
fc_prob <- fc %>% 
  as_tsibble() %>% 
  transmute(
    Quarter,
    q90 = quantile(Demand, 0.90),
    q95 = quantile(Demand, 0.95),
    q99 = quantile(Demand, 0.99)
  )

print("-----------------------")
