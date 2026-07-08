# change to your working directory
setwd("G:/My Drive/Mai/Teaching/courses/Forecasting/Fc-Excel-R-Python/")
library(tidyverse)
library(fpp3)
rm(list = ls())


###############################################
# Regression with ARIMA errors
# Example 1 New Car Sales
###############################################
library(readxl)  
d <- read_excel("06-TS-regression.xlsx", sheet = "NewCarSales")
glimpse(d)

ncs <- d %>% 
  mutate(
    YearQuarter = yearquarter("2002 Q1") + 0:59,
    qtr = factor(quarter(YearQuarter), levels = 1:4),
    qtr = relevel(qtr, ref = "1")
  ) %>% 
  select(-Date) %>% 
  relocate(YearQuarter) %>% 
  as_tsibble(index = YearQuarter)

ncs %>% 
  model(TSLM(NCS ~ DPIPC + UR + PR + UMICS)) %>% 
  gg_tsresiduals()

ncs %>% 
  model(ARIMA(NCS ~ DPIPC + UR + PR + UMICS)) %>% 
  gg_tsresiduals()

ncs %>% 
  model(ARIMA(NCS ~ DPIPC + UR + PR + UMICS)) %>% 
  report()


ncs %>% 
  model(regOLS = TSLM(NCS ~ DPIPC + UR + PR + UMICS),
        regARIMA = ARIMA(NCS ~ DPIPC + UR + PR + UMICS)) %>% 
  tidy()

ncs %>% 
  model(
    mTFM_NCS = ARIMA(NCS ~ pdq(0) + UR + lag(UR) + UMICS + lag(UMICS) + lag(UMICS, 2))
  ) %>% 
  tidy()



###############################################
# Regression with ARIMA errors
# Example 2 Consumption vs Income
###############################################
us_change %>%
  pivot_longer(c(Consumption, Income),
               names_to = "var", values_to = "value") %>%
  ggplot(aes(x = Quarter, y = value)) +
  geom_line() +
  facet_grid(vars(var), scales = "free_y") +
  labs(title = "US consumption and personal income",
       y = "Quarterly % change")

us_change %>%
  ggplot(aes(x = Income, y = Consumption)) +
  geom_point() +
  labs(x = "Income",
       y = "Consumption",
       title = "Quarterly changes in US consumption and personal income")

fitTSLM <- us_change %>% 
  model(tslm = TSLM(Consumption ~ Income))

report(fitTSLM)

fitTSLM %>% gg_tsresiduals()

fit <- us_change %>%
  model(ARIMA(Consumption ~ Income))
report(fit)






augment(fit) %>%
  features(.innov, ljung_box, dof = 5, lag = 8)

bind_rows(
  `Regression residuals` =
    as_tibble(residuals(fit, type = "regression")),
  `Innovations residuals` =
    as_tibble(residuals(fit, type = "innovation")),
  .id = "type"
) %>%
  mutate(
    type = factor(type, levels=c(
      "Regression residuals", "Innovations residuals"))
  ) %>%
  ggplot(aes(x = Quarter, y = .resid)) +
  geom_line() +
  facet_grid(vars(type))

us_change_future <- new_data(us_change, 8) %>%
  mutate(Income = mean(us_change$Income))

forecast(fit, new_data = us_change_future) %>%
  autoplot(us_change) +
  labs(y = "Percentage change")

forecast(fit, new_data = us_change_future)

forecast(fitTSLM, new_data = us_change_future)
forecast(fitTSLM, new_data = us_change_future) %>%
  autoplot(us_change) +
  labs(y = "Percentage change")



fitAll <- us_change %>% 
  model(reg = TSLM(Consumption ~ Income),
        dynreg = ARIMA(Consumption ~ Income))

tidy(fitAll)

fitAll %>% 
  select(reg) %>% 
  gg_tsresiduals()

fitAll %>% 
  select(dynreg) %>% 
  gg_tsresiduals()

fit %>% gg_tsresiduals()


###############################################
# Stochastic and deterministic trend
###############################################
aus_airpassengers |>
  autoplot(Passengers) +
  labs(y = "Passengers (millions)",
       title = "Total annual air passengers")

fit_deterministic <- aus_airpassengers |>
  model(deterministic = ARIMA(Passengers ~ 1 + trend() +
                                pdq(d = 0)))

fit_stochastic <- aus_airpassengers |>
  model(stochastic = ARIMA(Passengers ~ pdq(d = 1)))

aus_airpassengers |>
  autoplot(Passengers) +
  autolayer(fit_stochastic |> forecast(h = 20),
            colour = "#0072B2", level = 95) +
  autolayer(fit_deterministic |> forecast(h = 20),
            colour = "#D55E00", alpha = 0.65, level = 95) +
  labs(y = "Air passengers (millions)",
       title = "Forecasts from trend models")


# Train-test split for out-of-sample score
train <- aus_airpassengers |>
  filter_index(. ~ "2015")

test <- aus_airpassengers |>
  filter_index("2016" ~ .)

fit <- train |>
  model(
    OLSreg = TSLM(Passengers ~ trend()),
    deterministic = ARIMA(Passengers ~ 1 + trend() + pdq(d = 0)),
    stochastic    = ARIMA(Passengers ~ pdq(d = 1))
  )

fc <- fit |>
  forecast(new_data = test)


fc |>
  accuracy(
    test,
    measures = list(
      ME = ME,
      RMSE = RMSE,
      MAE = MAE,
      MAPE = MAPE,
      crps = CRPS
    )
  )


# in-class exercise

myts <- tsibble(
  Quarter = yearquarter("2001 Q1") + 0:11,
  Demand = c(190,260,330,480,570,710,650,690,730,680,660,590),
  index = Quarter
)

myts_train <- myts %>% 
  filter(year(Quarter) <= 2002)

myts_fit <- myts_train %>%
  model(
    Mean = MEAN(Demand),
    Naive = NAIVE(Demand),
    "Seasonal Naive" = SNAIVE(Demand),
    Drift = NAIVE(Demand ~ drift())
  )

myts_fc <- myts_fit %>% 
  forecast(h=4)

myts_fc %>% 
  accuracy(
    myts,
    measures = list(
      ME = ME, RMSE = RMSE, MAE = MAE, MAPE = MAPE
    )
  )





###############################################
# Transfer function model: lagged predictors
###############################################

# Sales and advertising
d <- read_csv("Blattberg_Jeuland1981.CSV")
glimpse(d)


dt <- d %>% 
  mutate(Month = yearmonth(Date)) %>% 
  relocate(Month) %>% 
  select(-Date) %>% 
  as_tsibble(index = Month)
dt


dt %>%
  pivot_longer(Sales:Advertising) %>%
  ggplot(aes(x = Month, y = value)) +
  geom_line() +
  facet_grid(vars(name), scales = "free_y") +
  labs(y = "", title = "Sales and advertising")

fit <- dt %>% 
  model(ARIMA(Sales ~ pdq(d=0) + Advertising +
                lag(Advertising) + lag(Advertising,2) +
                lag(Advertising,3) + lag(Advertising,4) + lag(Advertising,5)))
report(fit)




# insurance advertising

insurance %>%
  pivot_longer(Quotes:TVadverts) %>%
  ggplot(aes(x = Month, y = value)) +
  geom_line() +
  facet_grid(vars(name), scales = "free_y") +
  labs(y = "", title = "Insurance advertising and quotations")

insurance %>%
  # Restrict data so models use same fitting period
  mutate(Quotes = c(NA, NA, NA, Quotes[4:40]))

fit <- insurance %>%
  # Restrict data so models use same fitting period
  mutate(Quotes = c(NA, NA, NA, Quotes[4:40])) %>%
  
  # Estimate models
  model(
    lag0 = ARIMA(Quotes ~ pdq(d = 0) + TVadverts),
    lag1 = ARIMA(Quotes ~ pdq(d = 0) +
                   TVadverts + lag(TVadverts)),
    lag2 = ARIMA(Quotes ~ pdq(d = 0) +
                   TVadverts + lag(TVadverts) +
                   lag(TVadverts, 2)),
    lag3 = ARIMA(Quotes ~ pdq(d = 0) +
                   TVadverts + lag(TVadverts) +
                   lag(TVadverts, 2) + lag(TVadverts, 3))
  )

glance(fit)

fit_best <- insurance %>%
  model(ARIMA(Quotes ~ pdq(d = 0) +
                TVadverts + lag(TVadverts)))

report(fit_best)

insurance_future <- new_data(insurance, 20) %>%
  mutate(TVadverts = 8)

insurance_future

fit_best %>%
  forecast(insurance_future) %>%
  autoplot(insurance) +
  labs(
    y = "Quotes",
    title = "Forecast quotes with future advertising set to 8"
  )




###############################################
# Dynamic harmonic regression
###############################################

aus_cafe <- aus_retail %>%
  filter(
    Industry == "Cafes, restaurants and takeaway food services",
    year(Month) %in% 2004:2018
  ) %>%
  summarise(Turnover = sum(Turnover))

aus_cafe %>% 
  autoplot(Turnover)

fit <- model(aus_cafe,
             `K = 1` = ARIMA(log(Turnover) ~ fourier(K=1) + PDQ(0,0,0)),
             `K = 2` = ARIMA(log(Turnover) ~ fourier(K=2) + PDQ(0,0,0)),
             `K = 3` = ARIMA(log(Turnover) ~ fourier(K=3) + PDQ(0,0,0)),
             `K = 4` = ARIMA(log(Turnover) ~ fourier(K=4) + PDQ(0,0,0)),
             `K = 5` = ARIMA(log(Turnover) ~ fourier(K=5) + PDQ(0,0,0)),
             `K = 6` = ARIMA(log(Turnover) ~ fourier(K=6) + PDQ(0,0,0))
)

fit %>%
  forecast(h = "2 years") %>%
  autoplot(aus_cafe, level = 95) +
  facet_wrap(vars(.model), ncol = 2) +
  guides(colour = "none", fill = "none", level = "none") +
  geom_label(
    aes(x = yearmonth("2007 Jan"), y = 4250,
        label = paste0("AICc = ", format(AICc))),
    data = glance(fit)
  ) +
  labs(title= "Total monthly eating-out expenditure",
       y="$ billions")

fit %>% 
  glance() %>% 
  select(.model, AIC:BIC)

fit_best <- aus_cafe %>%
  model(ARIMA(log(Turnover) ~ fourier(K=6) + PDQ(0,0,0)))

fit_best %>%
  forecast(h = "2 years") %>%
  autoplot(aus_cafe, level = 95) +
  labs(title="Log transformed linear model with ARIMA(0,1,1) errors, Fourier K= 6",
       y = "$ billions")



###############################################
# ARIMAX
###############################################
fit_arimax <- us_change %>%
  model(ARIMA(Consumption ~ Income + pdq(1,0,0) + PDQ(0,0,0)))
report(fit_arimax)
