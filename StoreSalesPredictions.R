#Load libraries
library(tidyverse)
library(lubridate)
library(readr)
library(forecast)

#Read in data
train <- read_csv("../input/demand-forecasting-kernels-only/train.csv")
test <- read_csv("../input/demand-forecasting-kernels-only/test.csv")

#Remove unecessary variables
train.cleaned <- train %>% select(sales, store, item)

#Create multi-seasonal time series model and predict sales for the next 90 days. Seasonality aims to model weekly and yearly patterns.
df_total = data.frame()
for (y in 1:50){ #needs to be 1:50
  for (x in 1:10){ #needs to be 1:10
    traindata <- train.cleaned %>% filter(store == x) %>% filter(item == y) %>% select(sales)
    msts <- msts(data=traindata, seasonal.periods = c(7, 365.25), start = decimal_date(as.Date("2013-01-01")))
    tbats <- tbats(msts)
    predict <- predict(tbats, h=90)
    sales <- as.numeric(predict$mean)
    sales <- as.data.frame(sales)
    df_total <- rbind(df_total,sales)
  }
}

#Create new id variable
df_total$id <- seq.int(nrow(df_total))-1

#Reorder variables
submission <- df_total[c(2,1)]

#Write out submission file
write_csv(submission, "submission.csv")
