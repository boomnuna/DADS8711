# change to your working directory
setwd("G:/My Drive/Mai/Teaching/courses/Forecasting/Fc-Excel-R-Python/")

library(fpp3)
library(tidyverse)


#########################
# Intro
#########################

y <- tsibble(
  Quarter = yearquarter("2010 Q3") + 0:13,
  Obs = c(1212, 1321, 1278, 1341, 1257, 1341, 1257, 1287, 
          1189, 1111, 1145, 1150, 1298, 1331),
  index = Quarter
)

yTrain <- y %>% 
  filter(year(Quarter) <= 2012)

fit <- yTrain %>% 
  model(ets = ETS(Obs),
        mean = MEAN(Obs),
        naive = NAIVE(Obs)
        )

fit

fc <- fit %>% 
  forecast(h=4)
fc

fit %>% 
  forecast(h=4) %>% 
  autoplot(y, level=NULL)

accuracy(fc, y)

fit %>% 
  select(ets) %>% 
  report()

fc %>% 
  filter(.model=='ets')



yTrain %>% 
  model(ets = ETS(Obs),
        mean = MEAN(Obs),
        naive = NAIVE(Obs),
        ses = ETS(Obs ~ error("A") + trend("N") + season("N")),
        holt = ETS(Obs ~ error("A") + trend("A") + season("N")),
        hw = ETS(Obs ~ error("A") + trend("A") + season("A")),
  ) %>% 
  forecast(h=4) %>% 
  accuracy(y)




#########################
# SES
#########################
fit <- yTrain %>% 
  model(ann = ETS(Obs ~ error("A") + trend("Ad") + season("N")))

tidy(fit)
tidy(fit)$estimate

report(fit)

fc <- fit %>% 
  forecast(h=8)
fc

#########################
# Methods with trend
#########################




aus_economy <- global_economy |>
  filter(Code == "AUS") |>
  mutate(Pop = Population / 1e6)
autoplot(aus_economy, Pop) +
  labs(y = "Millions", title = "Australian population")

aus_economy |>
  model(
    `Holt's method` = ETS(Pop ~ error("A") +
                            trend("A") + season("N")),
    `Damped Holt's method` = ETS(Pop ~ error("A") +
                                   trend("Ad", phi = 0.9) + season("N"))
  ) |>
  forecast(h = 15) |>
  autoplot(aus_economy, level = NULL) +
  labs(title = "Australian population",
       y = "Millions") +
  guides(colour = guide_legend(title = "Forecast"))


#################

y %>%  model(
  "Holt" = ETS(Obs ~ error("A") + trend("A") + season("N")),
  "Damped Holt" = ETS(Obs ~ error("A") + trend("Ad", phi = 0.9) + season("N"))
) %>% 
  forecast(h=10) %>% 
  autoplot(y, level = NULL)


www_usage <- as_tsibble(WWWusage)
www_usage %>% autoplot(value) +
  labs(x="Minute", y="Number of users",
       title = "Internet usage per minute")

www_usage %>%
  slice(1:(n()-1)) %>% 
  stretch_tsibble(.init = 10, .step = 1) %>%
  model(
    SES = ETS(value ~ error("A") + trend("N") + season("N")),
    Holt = ETS(value ~ error("A") + trend("A") + season("N")),
    Damped = ETS(value ~ error("A") + trend("Ad") +
                   season("N"))
  ) %>%
  forecast(h = 1) %>%
  accuracy(www_usage)


fit <- www_usage %>%
  model(
    Damped = ETS(value ~ error("A") + trend("Ad") + season("N"))
  )

# Estimated parameters:
tidy(fit)

fit %>%
  forecast(h = 10) %>%
  autoplot(www_usage) +
  labs(x="Minute", y="Number of users",
       title = "Internet usage per minute")

############################
# Methods with seasonality
############################
aus_holidays <- tourism %>%
  filter(Purpose == "Holiday") %>%
  summarise(Trips = sum(Trips)/1e3)

fit <- aus_holidays %>%
  model(
    additive = ETS(Trips ~ error("A") + trend("A") + season("A")),
    multiplicative = ETS(Trips ~ error("M") + trend("A") + season("M"))
  )
fc <- fit %>% forecast(h = "3 years")
fc %>%
  autoplot(aus_holidays, level = NULL) +
  labs(title="Australian domestic tourism",
       y="Overnight trips (millions)") +
  guides(colour = guide_legend(title = "Forecast"))







#########################################
# MLE & Quantile score
#########################################
y <- tsibble(
  Quarter = yearquarter("2010 Q3") + 0:13,
  Obs = c(1212, 1321, 1278, 1341, 1257, 1341, 1257, 1287, 
          1189, 1111, 1145, 1150, 1298, 1331),
  index = Quarter
)

fit <- y %>% 
  filter(year(Quarter) <= 2012) %>% 
  model(myETS=ETS(Obs ~ error("A") + trend("N") + season("N")))
report(fit)

fc <- fit %>% 
  forecast(h=4) 
accuracy(fc, y, list(qs=quantile_score), probs=0.9)

fc %>% 
  autoplot(y, level=c(80, 90))



#########################################################
# Applications in inventory 
# Example: yearly demand of fan
#########################################################
demandVec <- c(30, 50, 30, 60, 10, 40, 30, 30, 20, 40) 

yFan <- tsibble(
  Year = 2001:2010,
  Demand = demandVec,
  index = Year
)

yFan %>% 
  autoplot(Demand)

yFan %>% 
  model(mMean = MEAN(Demand)) %>% 
  report()

# forecast
fc <- yFan %>% 
  model(mMean = MEAN(Demand)) %>% 
  forecast(h=1) 

# quantile forecast
fc %>% 
  as_tibble() %>% 
  transmute(
    Year,
    q90 = quantile(Demand, 0.90),
    q55 = quantile(Demand, 0.95),
    q99 = quantile(Demand, 0.99)
  )

fc <- yFan %>% 
  model(mMean = MEAN(Demand),
        mETS = ETS(Demand)) %>% 
  forecast(h=1) 

fc

# implication for inventory
yTrain <- yFan %>% 
  filter(Year <= 2007)

fit <- yTrain %>% 
  model(mEts = ETS(Demand),
        mNaive = NAIVE(Demand)) 


fit %>% 
  forecast(h=3) %>% 
  accuracy(yFan)


fit %>% 
  forecast(h=3) %>% 
  accuracy(yFan, list(qs=quantile_score), probs=0.95)
# try prob =0.5. Compare to MAE


fit %>% 
  forecast(h=1) 

# Based on above quantile score, we select ETS.
# ETS is used on the entire dataset
yFan %>% 
  model(mMean = ETS(Demand)) %>% 
  forecast(h=1) %>% 
  as_tibble() %>% 
  transmute(
    Year,
    q90 = quantile(Demand, 0.90),
    q95 = quantile(Demand, 0.95),
    q99 = quantile(Demand, 0.99)
  )



#########################################################
# Applications in inventory 
# Aggregate demand
# Example: Beer Australian production
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

beer_stats_formatted

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

