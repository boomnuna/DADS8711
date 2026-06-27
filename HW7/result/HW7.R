
# initial parameter 
library(readxl)
library(dplyr)
library(tsibble)
library(lubridate)
library(fpp3)


# load data
path <- file.choose()
df <- read_excel(path, sheet = "NewCarSales")
glimpse(df)

# filter specific data
ncs <- df %>% 
    mutate(
      YearQuarter = yearquarter("2002 Q1") + 0:59, # add column quarter continue until 60 quarter 
      qtr = factor(quarter(YearQuarter), levels = 1:4), # convert variable in to categorical data 
      qtr = relevel(qtr, ref = "1") # use to change baseline (mostly use in regression)
    ) %>% 
    select(-Date) %>% 
    relocate(YearQuarter) %>% 
    as_tsibble(index = YearQuarter)
ncs

# create model 
fit <- ncs %>% 
  model(
    model_1 = TSLM(NCS ~ DPIPC + UR + PR + UMICS),
    model_2 = TSLM(NCS ~ DPIPC + UR + PR + UMICS + season()), 
    model_3 = TSLM(NCS ~ DPIPC + UR + PR + UMICS + season() + trend()), 
    model_4 = TSLM(NCS ~ DPIPC + UR + PR + UMICS * season() + trend()),
  ) 

# evaluation 
fit %>% 
  glance() %>% # summarize fitted model
  select(.model, r_squared, adj_r_squared, AICc)  # AICc low is good


# show coefficient of model
fit %>% 
    select(model_3)%>% 
    report()

fit %>% 
  select(model_4)%>% 
  report()









