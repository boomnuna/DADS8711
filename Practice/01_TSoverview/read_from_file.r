library(tsibble)
library(fable)
library(readr)   
library(dplyr)
library(lubridate)

# load csv file
file_name <- file.choose()
df0 <- read_csv(file_name)

# convert to 
df1 <- df0 %>% 
    mutate(Quater = yearquarter(Date)) %>% 
    as_tsibble(key=c(StoreID, SKU) ,index = Quater) %>% 
    select(-Date)

# train/test split
train <- df1 %>% 
    filter(year(Quater) <= 2002)
test <- df1 %>% 
    filter(year(Quater) > 2002)

# model training 
fit <- train %>% 
    model(
        Mean = MEAN(Demand),
        Naive = NAIVE(Demand),
        SeasonNaive = SNAIVE(Demand),
        Drift = NAIVE(Demand ~ drift())
)

# train accuracy
accuracy(fit) %>% 
    arrange(RMSE)

# forecatsing train
fc <- fit %>%
    forecast(test)
View(fc)

# test accuracy
accuracy(fc, test) %>% 
    arrange(RMSE)
