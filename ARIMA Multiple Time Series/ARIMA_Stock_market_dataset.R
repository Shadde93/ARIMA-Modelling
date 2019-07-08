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


MyData <- read.csv(file="Sunspots_DL/GSPC.csv", header=FALSE, sep=",")
MyData<-MyData[,-c(1:4)]
MyData<-MyData[,-c(2:3)]
MyData<-as.ts(MyData)
MyData<-scale(MyData)

{
  data1 = window(MyData, start=1, end=601)
  train1 = window(MyData, start=1, end=501)
  test1 = window(MyData, start=502, end=602)
  Arimamodel <- auto.arima(train1)
  fore1 <- forecast(Arimamodel, h = 101)
  rmse1<-sqrt((sum((test1-fore1$mean)^2))*1/100)  
  
  data2 = window(MyData, start=501, end=1101)
  train2 = window(MyData, start=501, end=1000)
  test2 = window(MyData, start=1001, end=1101)
  Arimamodel<- Arima(train2, model = Arimamodel)
  fore2 <- forecast(Arimamodel, h = 101)
  rmse2<-sqrt((sum((test2-fore2$mean)^2))*1/100)  
  
  data3 = window(MyData, start=1001, end=1601)
  train3 = window(MyData, start=1001, end=1501)
  test3 = window(MyData, start=1502, end=1601)
  Arimamodel<- Arima(train3, model = Arimamodel)
  fore3 <- forecast(Arimamodel, h = 100)
  rmse3<-sqrt((sum((test3-fore3$mean)^2))*1/100) 
  
  data4 = window(MyData, start=1501, end=2101)
  train4 = window(MyData, start=1501, end=2001)
  test4 = window(MyData, start=2002, end=2101)
  Arimamodel<- Arima(train4, model = Arimamodel)
  fore4 <- forecast(Arimamodel, h = 100)
  rmse4<-sqrt((sum((test4-fore4$mean)^2))*1/100) 
  
  data5 = window(MyData, start=2001, end=2601)
  train5 = window(MyData, start=2001, end=2501)
  test5 = window(MyData, start=2502, end=2601)
  Arimamodel<- Arima(train5, model = Arimamodel)
  fore5 <- forecast(Arimamodel, h = 100)
  rmse5<-sqrt((sum((test5-fore5$mean)^2))*1/100) 
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
