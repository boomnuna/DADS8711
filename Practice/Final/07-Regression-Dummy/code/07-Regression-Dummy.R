# change to your working directory
setwd("G:/My Drive/Mai/Teaching/courses/Forecasting/Fc-Excel-R-Python/")
library(tidyverse)
library(readxl)  
library(fpp3)
rm(list = ls())

###############################################
# Example: Bank Salary
###############################################

d <- read_csv("banksalary.csv", col_types = "nffnnffnn")
glimpse(d)


de <- read_excel("07-Regression-Dummy.xlsx", sheet = "banksalary")
de$EducLev <- as.factor(de$EducLev)
glimpse(de)
class(de$EducLev)

summary(d)

d %>% 
  group_by(Gender) %>% 
  summarise(mean = mean(Salary))


d %>% 
  ggplot() +
  geom_point(mapping = aes(x=YrsExper, y=Salary, colour=Gender))

fit1 <- lm(Salary ~ YrsExper, data = d)
summary(fit1)

d %>% 
  ggplot() +
  geom_point(mapping = aes(x=YrsExper, y=Salary)) +
  geom_abline(intercept=30329.73, slope = 991.64) 


###############################################
# Dummy variable
###############################################
fit2 <- lm(Salary ~ YrsExper + Gender, data = d)
summary(fit2)

d %>% 
  ggplot() +
  geom_point(mapping = aes(x=YrsExper, y=Salary, colour=Gender)) +
  geom_abline(intercept=35823.8, slope = 981.2, colour="red") +
  geom_abline(intercept=27811.9, slope = 981.2, colour="darkolivegreen") 

# model selection
library(caret)

set.seed(1234)
model1 <- train(
  Salary ~ YrsExper, d,
  method = "lm",
  trControl = trainControl(method ="cv", number=5)
)
model1$resample %>% 
  summarise(RMSE=mean(RMSE), MAE=mean(MAE))


set.seed(1234)
model2



# Model selection
dt <- as_tsibble(d, index = Employee)
dt %>% 
  model(
    lm1 = TSLM(Salary ~ YrsExper),
    lm2 = TSLM(Salary ~ YrsExper + Gender)
  ) %>% 
  glance() %>% 
  select(.model, adj_r_squared, CV, AIC, AICc, BIC)

# Education level
m3 <- lm(Salary ~ YrsExper + YrsPrior + Gender + EducLev, d)
summary(m3)

as.numeric(m3$coefficients[1] + m3$coefficients["GenderFemale"] + m3$coefficients["EducLev5"])

d$Edu_1 = ifelse(d$EducLev == 1, 1, 0)
d$Edu_2 = ifelse(d$EducLev == 2, 1, 0)
d$Edu_3 = ifelse(d$EducLev == 3, 1, 0)
d$Edu_4 = ifelse(d$EducLev == 4, 1, 0)
d$Edu_5 = ifelse(d$EducLev == 5, 1, 0)

(lm(Salary ~ YrsExper + YrsPrior + Gender + Edu_1 + Edu_2 + Edu_4 + Edu_5, d))

(lm(Salary ~ YrsExper + YrsPrior + Gender + EducLev, d))



###############################################
# Seasonal dummy variable
############################################### 

recent_production <- aus_production %>% 
  filter(year(Quarter) >= 1992)

recent_production %>% 
  autoplot(Beer)

fit_beer <- recent_production %>% 
  model(TSLM(Beer ~ trend() + season()))
report(fit_beer)

augment(fit_beer) %>% 
  ggplot(aes(x = Beer, y = .fitted,
             colour = factor(quarter(Quarter)))) +
  geom_point() +
  labs(y = "Fitted", x = "Actual values",
       title = "Australian quarterly beer production") +
  geom_abline(intercept = 0, slope = 1) +
  guides(colour = guide_legend(title = "Quarter"))


fc <- fit_beer %>% forecast(h=12)
fc %>% 
  autoplot(recent_production)

###############################################
# Event modeling
############################################### 

dh <- read_csv("mustard.csv", col_types = "Dnf")
glimpse(dh)
dh <- dh %>% 
  mutate(Month = yearmonth(Date)) %>% 
  relocate(Month) %>% 
  select(-Date) %>% 
  as_tsibble(index = Month)

dfuture <- dh %>% 
  filter_index("2017 Jul"~.) %>% 
  select(-Mustard)
dts <- dh %>% 
  filter_index(.~"2017 Jun")

dts %>% autoplot(Mustard)

dts %>% 
  model(mTrendEvent = TSLM(Mustard ~ trend() + season())) %>% 
  report()


dts %>% 
  ggplot() +
  geom_boxplot(mapping = aes(x=EventIndex, y=Mustard))

fit <- dts %>% 
  model(mTrendEvent = TSLM(Mustard ~ trend() + EventIndex))
report(fit)

forecast(fit, new_data = dfuture)


# model selection
fit <- dts %>% 
  model(
    mod1 = TSLM(Mustard ~ EventIndex),
    mod2 = TSLM(Mustard ~ season()),
    mod3 = TSLM(Mustard ~ season() + EventIndex),
    mod4 = TSLM(Mustard ~ trend()), 
    mod5 = TSLM(Mustard ~ trend() + EventIndex),
    mod6 = TSLM(Mustard ~ trend() + season()),
    mod7 = TSLM(Mustard ~ trend() + season() + EventIndex)
  )

fit %>% 
  glance() %>% 
  select(c(.model, r_squared, adj_r_squared, AIC:BIC)) %>% 
  arrange(desc(adj_r_squared)) 


###############################################
# Intervention
############################################### 

souvenirs
souvenirs %>% autoplot(Sales)

souvenirs %>%  autoplot(log(Sales))

fit <- souvenirs %>% 
  mutate(festival = month(Month) == 3 & year(Month) >= 1988) %>% 
  model(reg = TSLM(log(Sales) ~ trend() + season() + festival))

report(fit)

souvenirs %>% 
  autoplot(Sales, col = "gray") +
  geom_line(data = augment(fit), aes(y = .fitted), col = "blue")

fit %>%  gg_tsresiduals()

augment(fit) %>% 
  features(.innov, ljung_box, lag = 24)


augment(fit) |>
  mutate(month = month(Month, label = TRUE)) |>
  ggplot(aes(x = month, y = .innov)) +
  geom_boxplot()


tidy(fit) |> mutate(pceffect = (exp(estimate) - 1) * 100)



future_souvenirs <- new_data(souvenirs, n = 36) |>
  mutate(festival = month(Month) == 3)
fit |>
  forecast(new_data = future_souvenirs) |>
  autoplot(souvenirs)


###############################################
# Interaction
############################################### 
m2i <- lm(Salary ~ YrsExper*Gender, data = d)
summary(m2i)

d %>% 
  ggplot() +
  geom_point(mapping = aes(x=YrsExper, y=Salary, colour=Gender)) +
  geom_abline(intercept= 30430.03, slope = 1527.76, colour="red") +
  geom_abline(intercept=34528.28, slope =  279.9634, colour="darkolivegreen") 



###############################################
# Regression: Matrix notation
############################################### 

dncs <- read_excel("07-TS-regression.xlsx", 
                   sheet = "NewCarSales")
X <- model.matrix(NCS ~ DPIPC + UR + PR + UMICS, dncs)
y <- data.matrix(dncs$NCS)
((solve(t(X) %*% X))%*%t(X)) %*%y


###############################################
# Homework (Optional)
############################################### 
aus_cafe <- aus_retail %>%
  filter(
    Industry == "Cafes, restaurants and takeaway food services",
    year(Month) %in% 2004:2018
  ) %>%
  summarise(Turnover = sum(Turnover))

aus_cafe %>% 
  autoplot(Turnover)




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