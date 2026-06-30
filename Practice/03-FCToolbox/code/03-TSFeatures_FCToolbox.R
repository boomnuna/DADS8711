# change to your working directory
#setwd("G:/My Drive/Mai/Teaching/courses/Forecasting/Fc-Excel-R-Python/")

library(fpp3)  
library(tidyverse)

##########################
# Lag plots
##########################

glimpse(aus_production)

recent_production <- aus_production %>%
  filter(year(Quarter) >= 2000)

autoplot(recent_production, Beer)  

recent_production %>%
  gg_lag(Beer, geom = "point") +
  labs(x = "lag(Beer, k)")



##########################
# ACF
##########################
y <- tsibble(
  Wk = 1:7,
  Sales = c(15, 10, 12, 16, 9, 12, 10),
  index = Wk
)

y %>% ACF(Sales, lag_max = 4)

recent_production %>% ACF(Beer, lag_max = 9)

recent_production %>%
  ACF(Beer) %>%
  autoplot() + labs(title="Australian beer production")


##########################
# White noise
##########################

set.seed(37)
y <- tsibble(sample = 1:50, wn = rnorm(50), index = sample)
y %>% autoplot(wn) + labs(title = "White noise", y = "")

y %>%
  ACF(wn) %>%
  autoplot() + labs(title = "White noise")


#####################################################
# Forecasting workflow: residual diagnostics
#####################################################

####################################
# Example 1: global economy
# tidy
gdppc <- global_economy |>
  mutate(GDP_per_capita = GDP / Population)

# visualize
gdppc |>
  filter(Country == "Sweden") |>
  autoplot(GDP_per_capita) +
  labs(y = "$US", title = "GDP per capita for Sweden")

# specify 
TSLM(GDP_per_capita ~ trend())

# estimate
fit <- gdppc |>
  model(trend_model = TSLM(GDP_per_capita ~ trend()))

fit

fit %>% 
  filter(Country == "Sweden") %>% 
  report()
  
# evaluate
fit %>% 
  filter(Country == "Sweden") %>% 
  gg_tsresiduals()

# forecast
fit |>
  forecast(h = "3 years") |>
  filter(Country == "Sweden") |>
  autoplot(gdppc) +
  labs(y = "$US", title = "GDP per capita for Sweden")

#################################################
# Example 2 google 
# Re-index based on trading days
google_stock <- gafa_stock |>
  filter(Symbol == "GOOG", year(Date) >= 2015) |>
  mutate(day = row_number()) |>
  update_tsibble(index = day, regular = TRUE)
# Filter the year of interest
google_2015 <- google_stock |> filter(year(Date) == 2015)
# Fit the models
google_fit <- google_2015 |>
  model(
    Mean = MEAN(Close),
    `Naïve` = NAIVE(Close),
    Drift = NAIVE(Close ~ drift())
  )
# Produce forecasts for the trading days in January 2016
google_jan_2016 <- google_stock |>
  filter(yearmonth(Date) == yearmonth("2016 Jan"))
google_fc <- google_fit |>
  forecast(new_data = google_jan_2016)
# Plot the forecasts
google_fc |>
  autoplot(google_2015, level = NULL) +
  autolayer(google_jan_2016, Close, colour = "black") +
  labs(y = "$US",
       title = "Google daily closing stock prices",
       subtitle = "(Jan 2015 - Jan 2016)") +
  guides(colour = guide_legend(title = "Forecast"))

# residual diagnostics
google_2015 |>
  model(NAIVE(Close)) |>
  gg_tsresiduals()



#####################################################
# Prediction intervals
#####################################################

y <- tsibble(
  Quarter = yearquarter("2010 Q3") + 0:13,
  Obs = c(1212, 1321, 1278, 1341, 1257, 1341, 1257, 1287, 
          1189, 1111, 1145, 1150, 1298, 1331),
  index = Quarter
)

y %>% 
  model(MEAN(Obs)) %>% 
  forecast(h=6) %>% 
  autoplot(y)


y %>% 
  model(NAIVE(Obs)) %>% 
  forecast(h=6) %>% 
  autoplot(y, level=c(80, 95))


#####################################################
# Forecasting using transformation
#####################################################
# small example
df <- read_csv("smallExample.csv")
head(df)
myts <- df %>% 
  mutate(Quarter = yearquarter(Date) ) %>% 
  as_tsibble(key = c(StoreID, SKU ), index = Quarter) %>% 
  filter(StoreID == 2, SKU == "A") %>% 
  select(-Date)

myts_train <- myts %>% 
  filter(year(Quarter) <= 2002) %>% 
  filter()

myts_fc <- myts_train %>%
  model(
    mNaiveLog = NAIVE(log(Demand)),
    mNaiveMean = MEAN(log(Demand))
  ) %>% 
  forecast(h=4, point_forecast = lst(median, mean)) 

myts_fc


