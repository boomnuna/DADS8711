# initial library
library(tsibbledata)
library(fable)
library(dplyr)

# load & select dataset 
pigs <- aus_livestock %>% 
  filter(Animal == "Pigs", State == "Victoria") %>% 
  mutate(Type="Actual")

# fit model
fit <- pigs %>%
  model(
    ets = ETS(Count ~ error("A") + trend("N") + season("N")))

# check co-efficient in model
report(fit)

# forecast for next 4 month
fc <- fit %>% forecast(h=4)

# create final table
forecast_tbl <- fc %>%
  as_tibble() %>%
  transmute(

    Month,
    Animal = "Pigs",
    State = "Victoria",
    Count = .mean,
    Type = "Forecast"
  )

# show result 
combined <- bind_rows(pigs, forecast_tbl)
tail(combined, n=6)

