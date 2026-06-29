# change to your working directory
setwd("G:/My Drive/Mai/Teaching/courses/Forecasting/Fc-Excel-R-Python/")
rm(list = ls())
library(fpp3)
library(tidyverse)

######################################################
# Fan yearly demand
######################################################

demandVec <- c(30, 50, 30, 60, 10, 40, 30, 30, 20, 40)
yFan <- tsibble(
  Year = 2001:2010,
  Demand = demandVec,
  index = Year
)

fit <- yFan %>% 
  model(mMean = MEAN(Demand)) 

report(fit)

fc <- fit %>% 
  forecast(h=1) 

fc



yFan %>% 
  autoplot(Demand)

hist(demandVec)



######################################################
# Small example
######################################################
df <- read_csv("smallExample.csv")
myts <- df %>% 
  mutate(Quarter = yearquarter(Date) ) %>% 
  as_tsibble(key = c(StoreID, SKU ), index = Quarter   ) %>% 
  select(-Date)

myts_train <- myts %>% 
  filter(year(Quarter) <= 2002)

# Fit model on training set
myts_fit <- myts_train %>%
  model(
    Mean = MEAN(Demand),
    Naive = NAIVE(Demand),
    "Seasonal Naive" = SNAIVE(Demand),
    Drift = NAIVE(Demand ~ drift())
  )
myts_fit


# Fit model on training set
myts_fit <- myts_train %>%
  model(
    Mean = MEAN(Demand),
    Naive = NAIVE(Demand),
    "Seasonal Naive" = SNAIVE(Demand),
    Drift = NAIVE(Demand ~ drift())
  )

# Forecast on test set
myts_fc <- myts_fit %>% 
  forecast(h=4)

myts_fc %>% 
  filter(StoreID == "2", SKU == "A") 


myts_fc %>% 
  filter(StoreID == "2", SKU == "A") %>% 
  accuracy(myts)



##############################################################
# Evaluation & Model Selection
##############################################################
accuracy(myts_fc, myts)  # Error on test

myts_fc %>% 
  filter(StoreID == "2", SKU == "A") %>% 
  accuracy(myts)  

myts_fc %>% 
  filter(StoreID == "2", SKU == "A") %>% 
  accuracy(myts, list(qs=quantile_score), probs=0.97)  # quantile score

myts_fc %>% 
  filter(StoreID == "2", SKU == "A") %>% 
  accuracy(myts, list(crps = CRPS))  # continuous ranked probability score


##############################################################
# Example
# Beer Australian production
##############################################################
recent_production <- aus_production %>%
  filter(year(Quarter) >= 2000)

# Suppose that we have selected two models, ETS and NN
set.seed(1)
beer_fit <- recent_production %>%
  model(
    ets = ETS(Beer),
    NN = NNETAR(Beer)
  )
beer_fc <- beer_fit %>% 
  forecast(h=2)

# distributional forecasts
library(distributional)
library(dplyr)

beer_stats <- beer_fc %>% 
  as_tibble() %>% 
  mutate(
    Mean     = mean(Beer),
    Variance = variance(Beer),   
    SD       = sqrt(Variance),   
    P97      = quantile(Beer, 0.97)
  ) %>% 
  select(.model, Quarter, Mean, Variance, SD, P97)

beer_stats_formatted <- beer_stats %>%
  mutate(across(where(is.numeric), ~ sprintf("%.2f", .)))

# making decision
decision_report <- beer_fc %>% 
  as_tibble() %>% 
  group_by(.model) %>% 
  summarise(
    Total_Demand_Dist = sum(Beer),
    Mean_Total = sum(.mean)
  ) %>% 
  mutate(
    Total_Variance = variance(Total_Demand_Dist),
    Total_SD = sqrt(Total_Variance),
    Required_Stock_97 = quantile(Total_Demand_Dist, 0.97)
  )

decision_report

# some decision may requires higher moments


