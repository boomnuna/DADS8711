# initial library
library(readxl)
library(dplyr)
library(lubridate)
library(tidyr)
library(fpp3) 
library(openxlsx)


# initial parameter
sheet_name <- "aus_production"
column_name <- c("Quarter", "Cement")

# load data
file_name <- file.choose() # choose 01-introTS-FC.xlsx
df0 <- read_excel(file_name, sheet=sheet_name)
df1 <- df0 %>% select(column_name)

# Convert to tsibble
df2 <- df1  %>% 
    mutate(Quarter = yearquarter(Quarter))  %>% 
    as_tsibble(index = Quarter)

# split data
train <- df2  %>% filter(year(Quarter) >= 1988 & year(Quarter) <= 2007)
test <- df2  %>% filter(year(Quarter) >= 2008)

# train model
fit <- train %>% 
    model(
        Mean = MEAN(Cement),
        Naive = NAIVE(Cement),
        SeasonNaive = SNAIVE(Cement),
        Drift = NAIVE(Cement ~ drift()),
        ETS = ETS(Cement),
        ARIMA = ARIMA(Cement)
    )

# train data evaluation
accuracy(fit) 

# forcasting
fc <- fit %>% forecast(h=nrow(test)) 

# test data evaluation
acc <- accuracy(fc, test)

# save the result
# ดึงค่า forecast ทุก model แล้ว pivot 
fc_all <- fc %>% 
  as_tibble() %>% 
  select(.model, Quarter, .mean) %>% 
  pivot_wider(names_from = .model, values_from = .mean)

# รวมกับ df2
df_result <- df2 %>% 
  left_join(fc_all, by = "Quarter")

# ดูผลลัพธ์
print(df_result)

# สร้าง workbook
wb <- createWorkbook()

# เพิ่ม sheet
addWorksheet(wb, "forecast")
addWorksheet(wb, "evaluation")

# เขียนข้อมูลลงแต่ละ sheet
writeData(wb, "forecast",   df_result)
writeData(wb, "evaluation", acc)

# save
saveWorkbook(wb, "result.xlsx", overwrite = TRUE)