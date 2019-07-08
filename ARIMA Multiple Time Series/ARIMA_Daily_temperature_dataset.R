# Core Tidyverse
library(tidyverse)
library(glue)
library(forcats)

# Time Series
library(timetk)
library(tidyquant)
library(tibbletime)

# Visualization
library(cowplot)

# Preprocessing
library(recipes)

# Sampling / Accuracy
library(rsample)
library(yardstick) 

# Modeling
library(keras)
library(forecast)
library(tseries)
library(stats)


MyData <- read.csv(file="Sunspots_DL/daily-minimum-temperatures-in-me.csv", header=FALSE, sep=",")
MyData<-MyData[,-1]
MyData<-as.ts(MyData)

{
  data1 = window(MyData, start=1, end=1097)
  train1 = window(MyData, start=1, end=732)
  test1 = window(MyData, start=733, end=1097)
  Arimamodel <- auto.arima(train1)
  f<-predict(Arimamodel, n.ahead = 365)
  fore1 <- forecast(Arimamodel, h = 365)
  rmse1<-sqrt((sum((test1-fore1$mean)^2))*1/365)  
  
  data2 = window(MyData, start=518, end=1613)
  train2 = window(MyData, start=518, end=1249)
  test2 = window(MyData, start=1250, end=1613)
  Arimamodel<- Arima(train2, model = Arimamodel)
  fore2 <- forecast(Arimamodel, h = 365)
  rmse2<-sqrt((sum((test2-fore2$mean)^2))*1/365)  
  
  data3 = window(MyData, start=1097, end=2191)
  train3 = window(MyData, start=1097, end=1827)
  test3 = window(MyData, start=1828, end=2191)
  Arimamodel<- Arima(train3, model = Arimamodel)
  fore3 <- forecast(Arimamodel, h = 365)
  rmse3<-sqrt((sum((test3-fore3$mean)^2))*1/365) 
  
  data4 = window(MyData, start=1613, end=2709)
  train4 = window(MyData, start=1613, end=2343)
  test4 = window(MyData, start=2343, end=2709)
  Arimamodel<- Arima(train4, model = Arimamodel)
  fore4 <- forecast(Arimamodel, h = 365)
  rmse4<-sqrt((sum((test4-fore4$mean)^2))*1/365) 
  
  data5 = window(MyData, start=2191, end=3287)
  train5 = window(MyData, start=2191, end=2922)
  test5 = window(MyData, start=2923, end=3287)
  Arimamodel<- Arima(train5, model = Arimamodel)
  fore5 <- forecast(Arimamodel, h = 365)
  rmse5<-sqrt((sum((test5-fore5$mean)^2))*1/365) 
}


{
  par(mfrow=c(2,3))
  
  seqplot.ts(data1,fore1$mean, xlab = "Time", ylab = "Value", main = c("Slice01, RMSE", round(rmse1, digits = 1)))
  
  seqplot.ts(data2,fore2$mean, xlab = "Time", ylab = "Value", main = c("Slice02, RMSE", round(rmse2, digits = 1)))
  
  seqplot.ts(data3,fore3$mean, xlab = "Time", ylab = "Value", main = c("Slice03, RMSE", round(rmse3, digits = 1)))
  
  seqplot.ts(data4,fore4$mean, xlab = "Time", ylab = "Value", main = c("Slice04, RMSE", round(rmse4, digits = 1)))
  
  seqplot.ts(data5,fore5$mean, xlab = "Time", ylab = "Value", main = c("Slice05, RMSE", round(rmse5, digits = 1)))
  
}

rmse_vec<-c(rmse1,rmse2,rmse3,rmse4,rmse5)

avg_RMSE_mean <- mean(rmse_vec)
avg_RMSE_mean
sd_RMSE <- sd(rmse_vec)
sd_RMSE
