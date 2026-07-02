# # change to your working directory
# setwd("G:/My Drive/Mai/Teaching/courses/Forecasting/Fc-Excel-R-Python/")
library(fpp3)
library(tidyverse)

us_retail_employment <- us_employment %>%
  filter(year(Month) >= 1990, Title == "Retail Trade") %>%
  select(-Series_ID)


##################################################
# Decomposition & Forecasting: Simple Approach
# R
##################################################
y <- tsibble(
  Quarter = yearquarter("2001 Q1") + 0:15,
  Obs = c(115, 90, 65, 135, 130, 95, 75, 150, 
          135, 105, 85, 155, 145, 110, 85, 160),
  index = Quarter
)

fit_y <- y %>% 
  model(mSTLNaive = decomposition_model(STL(Obs~ season(window = Inf)),
                                        NAIVE(season_adjust)),
        
        mSTLDft = decomposition_model(STL(Obs~ season(window = Inf)),
                                      NAIVE(season_adjust~drift()))
)

fit_y %>% 
  forecast(h=12) %>% 
  autoplot(y, level = NULL)

fc <- fit_y %>% 
  forecast(h=12, point_forecast = lst(median, mean)) 
#write.csv(fc, "temp.csv")

#########################
# Moving average
#########################

global_economy %>%
  filter(Country == "Australia") %>%
  autoplot(Exports) +
  labs(y = "% of GDP", title = "Total Australian exports")

aus_exports <- global_economy %>%
  filter(Country == "Australia") %>%
  mutate(
    `5-MA` = slider::slide_dbl(Exports, mean,
                               .before = 2, .after = 2, .complete = TRUE)
)

aus_exports %>%
  autoplot(Exports) +
  geom_line(aes(y = `5-MA`), colour = "#D55E00") +
  labs(y = "% of GDP",
       title = "Total Australian exports") +
  guides(colour = guide_legend(title = "series"))

#########################
# Components of TS
#########################
y <- tsibble(
  Quarter = yearquarter("2001 Q1") + 0:15,
  Obs = c(115, 90, 65, 135, 130, 95, 75, 150, 
          135, 105, 85, 155, 145, 110, 85, 160),
  index = Quarter
)

autoplot(y)

fit <- y %>%
  model(
    ClassicalNaive =
      decomposition_model(
        classical_decomposition(Obs, type = "additive"),
        NAIVE(season_adjust)
      )
  )

fc <- fit %>%
  forecast(h = 12)


  
dcmp <- y %>%
  model(mM = classical_decomposition(Obs, type = "multiplicative"),
        mA = classical_decomposition(Obs, type = "additive")
  ) %>% 
  components() 



cmp %>% d
  filter(.model == "mA")

dcmp %>% 
  filter(.model == "mA") %>% 
  autoplot()

filter(dcmp, .model == "mM")
filter(dcmp, .model == "mA")

# additive classical decomposition 
us_retail_employment %>%
  model(
    mA = classical_decomposition(Employed, type = "additive")
  ) %>%
  components() %>%
  autoplot() +
  labs(title = "Classical decomposition of total
                  US retail employment")

us_retail_employment %>%
  model(
    mSTL = STL(Employed)
  ) %>%
  components() %>%
  autoplot() +
  labs(title = "STL decomposition of total
                  US retail employment")



# STL
dcmp <- us_retail_employment %>%
  model(stl = STL(Employed))

components(dcmp) %>% autoplot()

tail(components(dcmp))

us_retail_employment %>%
  model(
    STL(Employed ~ trend(window = 7) +
          season(window = "periodic"),
        robust = TRUE)) %>%
  components() %>%
  autoplot()

components(dcmp) %>%
  as_tsibble() %>%
  autoplot(Employed, colour = "gray") +
  geom_line(aes(y=season_adjust), colour = "#0072B2") +
  labs(y = "Persons (thousands)",
       title = "Total employment in US retail")



#############################
# Decomposition & Forecast
############################
y <- tsibble(
  Quarter = yearquarter("2001 Q1") + 0:15,
  Obs = c(115, 90, 65, 135, 130, 95, 75, 150, 
          135, 105, 85, 155, 145, 110, 85, 160),
  index = Quarter
)

dcmp <- y %>%
  model(ClassicalAdditive = classical_decomposition(Obs, 
                                      type = "additive"))

y %>%
  model(mCAdd = classical_decomposition(Obs, type = "additive")) %>% 
  components() %>% 
  autoplot()


y %>%
  model(mSTL = STL(Obs~ season(window = Inf))) %>% 
  components() 

y %>%
  model(mSTL = STL(Obs)) %>% 
  components() 


fit_y <- y %>% 
  model(mSTLNaive = decomposition_model(STL(Obs~ season(window = 13)),
                                        NAIVE(season_adjust))
  ) 

result <- fit_y %>% 
  forecast(h=4) 

y %>% 
  model(mSTLNaive = decomposition_model(STL(Obs~ season(window = Inf)),
                                        NAIVE(season_adjust))
  ) %>% 
  forecast(h=4)

fit_y %>% 
  forecast(h=12) %>% 
  autoplot(y, level = NULL)


fit_y <- y %>% 
  model(mSTLNaive = decomposition_model(STL(Obs~ season(window = Inf)),
                                        NAIVE(season_adjust)),
        
        mSTLDft = decomposition_model(STL(Obs~ season(window = Inf)),
                                      NAIVE(season_adjust~drift()))
  )
fit_y %>% 
  forecast(h=12) %>% 
  autoplot(y, level = NULL)


us_retail_employment %>%
  model(
    STL(Employed ~ trend(window = 7) +
          season(window = "periodic"),
        robust = TRUE)) %>%
  components() %>%
  autoplot()


#############################
# TS Features
############################

# lag plot
glimpse(aus_production)

recent_production <- aus_production %>%
  filter(year(Quarter) >= 2000)

autoplot(recent_production, Beer)  

recent_production %>%
  gg_lag(Beer, geom = "point") +
  labs(x = "lag(Beer, k)")

# autocorrelation function
y <- tsibble(
  Wk = 1:7,
  Sales = c(15, 10, 12, 16, 9, 12, 10),
  index = Wk
)
y %>% ACF(Sales, lag_max = 4) %>% 
autoplot()

recent_production %>%
  ACF(Beer, lag_max = 4)
  
recent_production %>%
  ACF(Beer) %>%
  autoplot() + labs(title="Australian beer production")

# white noise
set.seed(37)
y <- tsibble(sample = 1:50, wn = rnorm(50), index = sample)
y %>% autoplot(wn) + labs(title = "White noise", y = "")


