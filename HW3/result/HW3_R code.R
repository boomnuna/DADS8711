# initial library
library(readxl)
library(dplyr)
library(lubridate)
library(tidyr)
library(fpp3) 
library(openxlsx)


# initial parameter
sheet_name <- "aus_production"
column_name <- c("Quarter", "Cement")

# load data
file_name <- file.choose() # choose 01-introTS-FC.xlsx
df0 <- read_excel(file_name, sheet=sheet_name)
df1 <- df0 %>% select(column_name)

# Convert to tsibble
df2 <- df1  %>% 
    mutate(Quarter = yearquarter(Quarter))  %>% 
    as_tsibble(index = Quarter)

# split data
train <- df2 %>%
        filter(year(Quarter) >= 1988) %>% 
          slice(1:(n()-1)) %>% 
            stretch_tsibble(.init = 8, .step=1)

# total resample
train %>% n_keys()

# train model
fit <- train %>% 
    model(
        Mean = MEAN(Cement),
        Naive = NAIVE(Cement),
        SeasonNaive = SNAIVE(Cement),
        Drift = NAIVE(Cement ~ drift()),
        ETS = ETS(Cement),
        ARIMA = ARIMA(Cement)
    ) %>% 
    forecast(h=1) %>% 
    accuracy(df2) 
    #accuracy(df2, by = c(".model", ".id")) 

# find best model
fit %>% 
  arrange(RMSE) %>% 
  View()

