library(tsibble)
library(fable)
library(feasts)
library(tidyverse)

# data
deman_vec <- c(30,50,30,60,10,40,30,30,20,40)

 # create time series dataframe
yFan <- tsibble(
    Year = 2001:2010,
    Demand = deman_vec,
    index = Year
)

# model training 
fit <- yFan %>% 
    model(Mean = MEAN(Demand),
    Naive = NAIVE(Demand),
    SeasonNaive = SNAIVE(Demand),
    Drift = NAIVE(Demand ~ drift())
    ) 

# forecasting
fc <- fit %>% 
    forecast(h=2)

fc