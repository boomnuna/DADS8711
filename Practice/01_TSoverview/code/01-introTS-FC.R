# change to your working directory
setwd("G:/My Drive/Mai/Teaching/courses/Forecasting/Fc-Excel-R-Python/")

library(fpp3)
library(tidyverse)

#########################
# tsibble
#########################

y <- tsibble(
  Year = 2015:2019,
  Observation = c(123, 39, 78, 52, 110),
  index = Year
)

y

demand_val <- c(190, 260, 330, 480, 570, 710, 650, 690, 730, 680, 660, 590)
Quarter <- yearquarter("2001 Q1") + 0:11
Quarter

dts <- tsibble(
  Quarter = yearquarter("2001 Q1") + 0:11,
  Demand = demand_val,
  index = Quarter
)



tute1 <- read_csv("tute1.csv")

mytimeseries <- tute1 %>% 
  mutate(Quarter = yearquarter(Quarter)) %>% 
  as_tsibble(index = Quarter)

head(mytimeseries)


olympic_running
aus_production


?olympic_running

olympic_running %>% 
  distinct(Sex, Length)

##############################
# small examples
# Train & test 
###############################

df <- read_csv("smallExample.csv")
head(df)
tail(df)
glimpse(df)

myts <- df %>% 
  mutate(Quarter = yearquarter(Date) ) %>% 
  as_tsibble(key = c(StoreID, SKU ), index = Quarter   ) %>% 
  select(-Date)

# Create training set
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

accuracy(myts_fit)  # Residual on train

# Forecast on test set
myts_fc <- myts_fit %>% 
  forecast(h=4)
myts_fc

accuracy(myts_fc, myts)  # Error on test

accuracy(myts_fc, myts)  %>% 
  filter(StoreID == "2") %>% 
  filter(SKU == "A")

# write forecast to CSV file
write.csv(myts_fc, "temp.csv", row.names = FALSE)






#########################
# Cross validation
#########################
myts_trCV <- myts %>% 
  filter(StoreID == 2, SKU == "A") %>%
  slice(1:(n()-1)) %>% 
  stretch_tsibble(.init=3, .step=1) %>% 
  relocate(.id, Quarter) 


myts_trCV %>% 
  model(
    Mean = MEAN(Demand),
    Naive = NAIVE(Demand)
  ) %>% 
  forecast(h=1) %>% 
  accuracy(myts) 

# details...
myts_trCV %>% 
  filter(.id %in% c(1,2, 3))



#########################
# Probabilistic forecast
#########################
demandVec <- c(30, 50, 30, 60, 10, 40, 
               30, 30, 20, 40)
yFan <- tsibble(
  Year = 2001:2010,
  Demand = demandVec,
  index = Year
)

fc <- yFan %>% 
  model(mMean = MEAN(Demand)) %>% 
  forecast(h=1) 

fc

fc %>% 
  as_tibble() %>% 
  transmute(
    Year,
    q90 = quantile(Demand, 0.90),
    q95 = quantile(Demand, 0.95),
    q99 = quantile(Demand, 0.99)
  )

