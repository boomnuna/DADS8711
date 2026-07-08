# # change to your working directory
# setwd("G:/My Drive/Mai/Teaching/courses/Forecasting/Fc-Excel-R-Python/")

library(fpp3)
library(tidyverse)

#######################################################
# Example: Corticosteroid drug sales in Australia
############################nm #########################
help(PBS)

h02 <- PBS %>%
  filter(ATC2 == "H02") %>%
  summarise(Cost = sum(Cost)/1e6)

h02 %>%
  mutate(log(Cost)) %>%
  pivot_longer(-Month) %>%
  ggplot(aes(x = Month, y = value)) +
  geom_line() +
  facet_grid(name ~ ., scales = "free_y") +
  labs(y="", title="Corticosteroid drug scripts (H02)")

# ARIMA overview

h02 <- PBS %>%
  filter(ATC2 == "H02") %>%
  summarise(Cost = sum(Cost)/1e6)
fit <- h02 %>% 
  model(auto = ARIMA(log(Cost)))
report(fit)
fit %>% gg_tsresiduals(lag_max=36)



#######################################################
# Non-seasonal ARIMA
#####################################################
fit <- global_economy |>
  filter(Code == "EGY") |>
  model(ARIMA(Exports))
report(fit)


fit2 <- global_economy |>
  filter(Code == "EGY") |>
  model(ARIMA(Exports ~ pdq(4,1,2)))
report(fit2)

fit %>% 
  forecast(h = 10) %>% 
  autoplot(global_economy)



#######################################################
# NZ Arrivals
#####################################################

# NZ arrivals
nzarrivals <- aus_arrivals %>% filter(Origin == "NZ")

nzarrivals %>% autoplot(Arrivals / 1e3) + labs(y = "Thousands of people")

fit <- nzarrivals %>% 
  model(arima = ARIMA(Arrivals))

report(fit)
fit %>%  gg_tsresiduals()

#######################################################
# ARIMA vs ETS
#######################################################

# Example Aus population
aus_economy <- global_economy %>%
  filter(Code == "AUS") %>%
  mutate(Population = Population/1e6)


aus_economy %>%
  slice(-n()) %>%
  stretch_tsibble(.init = 10) %>%
  model(
    ETS(Population),
    ARIMA(Population)
  ) %>%
  forecast(h = 1) %>%
  accuracy(aus_economy) %>%
  select(.model, RMSE:MAPE)

aus_economy %>%
  model(ETS(Population)) %>%
  forecast(h = "5 years") %>%
  autoplot(aus_economy %>% filter(Year >= 2000)) +
  labs(title = "Australian population",
       y = "People (millions)")



###############################################
# Application
# Warehouse location 
###############################################

df <- global_economy %>% 
  filter(Code %in% c('THA','VNM','MMR','LAO')) %>% 
  mutate(Population = Population/1e6) 


accTable <- df %>%
  filter(Year >= 2000 & Year < 2017) %>% 
  stretch_tsibble(.init = 10) %>%   # Try .init = 3. ETS fails
  model(
    ETS(Population),
    ARIMA(Population)
  ) %>%
  forecast(h = 1) %>%
  accuracy(df) 

accTable %>% 
  select(c(.model, Country, ME:MASE))

accTable %>%
  group_by(.model) %>%
  summarise(avg_MAE = mean(MAE, na.rm = TRUE)) %>%
  arrange(avg_MAE)

fc <- df %>%
  model(ETS(Population)) %>% 
  forecast(h=1) %>% 
  mutate(FC2018 = mean(Population)) %>%  # pull point FC (mean of distr FC)
  as_tibble() %>% 
  select(Country, Year, FC2018)
fc

coords <- data.frame(
  Country = c("Thailand", "Vietnam", "Lao PDR", "Myanmar"),
  lat     = c(13.7563, 10.8231, 17.9757, 16.8409),
  lon     = c(100.5018, 106.6297, 102.6331, 96.1735)
)
input <- fc %>% 
  left_join(coords, by = "Country")
input










