# change to your working directory
setwd("G:/My Drive/Mai/Teaching/courses/Forecasting/Fc-Excel-R-Python/")
library(tidyverse)
library(readxl)  
library(fpp3)
rm(list = ls())


###############################################
# Multiple regression
###############################################

d <- read_excel("06-TS-regression.xlsx", sheet = "NewCarSales")
glimpse(d)

ncs <- d %>% 
  mutate(Quarter = yearquarter("2002 Q1") + 0:59) %>% 
  select(-Date) %>% 
  relocate(Quarter) %>% 
  as_tsibble(index = Quarter) 

ncs %>% autoplot(NCS)

ncs %>% 
  pivot_longer(-Quarter) %>% 
  ggplot(aes(x= Quarter, y = value, color = name)) +
  geom_line() +
  facet_grid(name~., scales = "free_y")  # try without facet


ncs %>% 
  GGally::ggpairs(columns = 2:6)

fit <- ncs %>% 
  model(tslm = TSLM(NCS ~ DPIPC + UR + PR + UMICS))
report(fit)

augment(fit)
ncs[1,]




###############################################
# TS forecasting with regression
###############################################
# assume we use seasonal naive. Other methods can be used.
DPIPC.base <- ncs %>% 
  model(SNAIVE(DPIPC)) %>% 
  forecast(h=4) %>% 
  pull(.mean)
DPIPC.base

UR.base <- ncs %>% 
  model(SNAIVE(UR)) %>% 
  forecast(h=4) %>% 
  pull(.mean)

PR.base <- ncs %>% 
  model(SNAIVE(PR)) %>% 
  forecast(h=4) %>% 
  pull(.mean)

UMICS.base <- ncs %>% 
  model(SNAIVE(UMICS)) %>% 
  forecast(h=4) %>% 
  pull(.mean)

baseline <- tsibble(
  Quarter = yearquarter("2017 Q1") + 0:3,
  DPIPC = DPIPC.base,
  PR = PR.base,
  UMICS = UMICS.base,
  UR = UR.base,
  index = Quarter
)
baseline

fc <- forecast(fit, new_data = baseline)
fc

ncs %>% 
  autoplot(NCS) +
  autolayer(fc)


# scenario based forecasting
future_scenarios <- scenarios(
  Baseline = new_data(ncs, 4) %>% 
    mutate(
      DPIPC = DPIPC.base,
      PR = PR.base,
      UMICS = UMICS.base,
      UR = UR.base
    ),
  Increase = new_data(ncs, 4) %>% 
    mutate(
      DPIPC = 1.05*DPIPC.base,
      PR = PR.base,
      UMICS = 1.03*UMICS.base,
      UR = 0.98*UR.base,
    )
)
fc <- forecast(fit, new_data = future_scenarios)
ncs %>% 
  autoplot(NCS) +
  autolayer(fc)

# cross validation to select fc method for predictor
# "unpivot": long format 
ncs_long <- ncs %>% 
  select(-NCS) %>% 
  pivot_longer(!Quarter, names_to="variable", values_to = "value")
ncs_long

trCV <- ncs_long %>% 
  slice(1:(n()-4*4)) %>% 
  stretch_tsibble(.init=6, .step=1) %>% 
  relocate(.id, Quarter) 

trCV %>% 
  model(
    Mean = MEAN(value),
    Naive = NAIVE(value),
    Drift = NAIVE(value ~ drift()),
    Snaive = SNAIVE(value)
  ) %>% 
  forecast(h=4) %>% 
  accuracy(ncs_long) %>% 
  arrange(variable, RMSE)

###############################################
# diagnostic test
###############################################

fit <- ncs %>% 
  model(tslm = TSLM(NCS ~ DPIPC + UR + PR + UMICS))

fit %>% report()

fit %>% gg_tsresiduals()

# Residual plots against predictors
ncs %>% 
  left_join(residuals(fit), by = "Quarter") %>% 
  pivot_longer(DPIPC:UMICS,
               names_to = "regressor", values_to = "x") %>% 
  ggplot() +
  geom_point(aes(x=x, y=.resid)) +
  facet_wrap(. ~regressor, scales = "free_x") +
  labs(y="Residuals", x="")

# Residual plots against fitted values
augment(fit) %>% 
  ggplot() +
  geom_point(aes(x=.fitted, y = .resid)) +
  labs(x="Fitted", y="Residuals")


augment(fit) %>% 
  features(.innov, ljung_box, lag=10)

ncs %>% 
  GGally::ggpairs(columns = 3:6)


###############################################
# model selection
###############################################
ncs %>% 
  mutate(URPR = UR*PR) %>% 
  model(
    tslm0 = TSLM(NCS ~ DPIPC),
    tslm1 = TSLM(NCS ~ DPIPC + UR + PR + UMICS),
    tslm2 = TSLM(NCS ~ DPIPC + UR + PR + UMICS + URPR)
    ) %>% 
  glance() %>% 
  select(.model, adj_r_squared, CV, AIC, AICc, BIC)

###############################################
# Time series feature
###############################################
tourism_features <- tourism |>
  features(Trips, feature_set(pkgs = "feasts"))
tourism_features
write.csv(tourism_features, "temp.csv")

library(glue)
tourism_features |>
  select_at(vars(contains("season"), Purpose)) |>
  mutate(
    seasonal_peak_year = seasonal_peak_year +
      4*(seasonal_peak_year==0),
    seasonal_trough_year = seasonal_trough_year +
      4*(seasonal_trough_year==0),
    seasonal_peak_year = glue("Q{seasonal_peak_year}"),
    seasonal_trough_year = glue("Q{seasonal_trough_year}"),
  ) |>
  GGally::ggpairs(mapping = aes(colour = Purpose))


library(broom)
pcs <- tourism_features |>
  select(-State, -Region, -Purpose) |>
  prcomp(scale = TRUE) |>
  augment(tourism_features)
pcs |>
  ggplot(aes(x = .fittedPC1, y = .fittedPC2, col = Purpose)) +
  geom_point() +
  theme(aspect.ratio = 1)
###############################################
# Simple linear regression
###############################################

d <- read_excel("09-TS-regression.xlsx", sheet = "JewelryData")
glimpse(d)

jewelry <- d %>% 
  mutate( Quarter = yearquarter("2010 Q1") + 0:27 ) %>% 
  select(c(Quarter, DPI, JS)) %>% 
  as_tsibble(index = Quarter)

jewelry %>% 
  pivot_longer(c(JS, DPI), names_to = "Series") %>% 
  autoplot(value) 

jewelry %>% 
  ggplot() +
  geom_point(aes(x=DPI, y=JS))

# train & test split
jewelry_tr <- jewelry %>%
  slice(1:(n() - 4))

js_dcmp <- jewelry_tr %>% 
  model(classical_decomposition(JS, type = "multiplicative")) %>% 
  components() %>% select(-.model) %>% as_tsibble()

jewelry_tr <- jewelry_tr %>% 
  mutate(JSSA = js_dcmp$season_adjust)

jewelry_tr %>% 
  ggplot() +
  geom_point(aes(x=DPI, y=JSSA))

jewelry_tr <- jewelry_tr %>% 
  mutate(JSSA = js_dcmp$season_adjust)

jewelry_tr %>% 
  model(TSLM(JSSA ~ DPI)) %>% 
  report()

fit <- jewelry_tr %>% 
  model(TSLM(JSSA ~ DPI)) 

coef(fit)$estimate 



###############################################
# Homework (optinal)
###############################################
us_change

help(us_change)

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


###############################################
# Homework New Car Sales
###############################################

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
  model(
    mod1 = TSLM(NCS ~ DPIPC + UR + PR + UMICS),
    mod2 = TSLM(NCS ~ DPIPC + UR + PR + UMICS + season())
  ) %>% 
  glance() %>% 
  select(.model, r_squared, adj_r_squared, AICc)


ncs %>% 
  model(TSLM(NCS ~ DPIPC + UR + PR + UMICS + season())) %>% 
  report()
  