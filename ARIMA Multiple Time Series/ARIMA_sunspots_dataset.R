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


data <- datasets::sunspot.month

{
data1 = window(data, start=c(1749,1), end=c(1808,12))
train1 = window(data, start=c(1749,1), end=c(1799,1))
test1 = window(data, start=c(1799,2), end=c(1808,12))
Arimamodel <- auto.arima(train1)
fore1 <- forecast(Arimamodel, h = 119)
rmse1<-sqrt((sum((test1-fore1$mean)^2))*1/119)

data2 = window(data, start=c(1769,2), end=c(1829,1))
train2 = window(data, start=c(1769,2), end=c(1819,1))
test2 = window(data, start=c(1819,3), end=c(1829,1))
Arimamodel<- Arima(train2, model = Arimamodel)
fore2 <- forecast(Arimamodel, h = 119)
rmse2<-sqrt((sum((test2-fore2$mean)^2))*1/119)

data3 = window(data, start=c(1789,3), end=c(1849,2))
train3 = window(data, start=c(1789,3), end=c(1839,3))
test3 = window(data, start=c(1839,4), end=c(1849,2))
Arimamodel<- Arima(train3, model = Arimamodel)
fore3 <- forecast(Arimamodel, h = 119)
rmse3<-sqrt((sum((test3-fore3$mean)^2))*1/119)

data4 = window(data, start=c(1809,4), end=c(1869,3))
train4 = window(data, start=c(1809,4), end=c(1859,4))
test4 = window(data, start=c(1859,5), end=c(1869,3))
Arimamodel<- Arima(train4, model = Arimamodel)
fore4 <- forecast(Arimamodel, h = 119)
rmse4<-sqrt((sum((test4-fore4$mean)^2))*1/119)

data5 = window(data, start=c(1829,5), end=c(1889,4))
train5 = window(data, start=c(1829,5), end=c(1879,5))
test5 = window(data, start=c(1879,6), end=c(1889,4))
Arimamodel<- Arima(train5, model = Arimamodel)
fore5 <- forecast(Arimamodel, h = 119)
rmse5<-sqrt((sum((test5-fore5$mean)^2))*1/119)

data6 = window(data, start=c(1849,6), end=c(1909,5))
train6 = window(data, start=c(1849,6), end=c(1899,6))
test6 = window(data, start=c(1899,7), end=c(1909,5))
Arimamodel<- Arima(train6, model = Arimamodel)
fore6 <- forecast(Arimamodel, h = 119)
rmse6<-sqrt((sum((test6-fore6$mean)^2))*1/119)

data7 = window(data, start=c(1869,7), end=c(1929,6))
train7 = window(data, start=c(1869,7), end=c(1919,7))
test7 = window(data, start=c(1919,8), end=c(1929,6))
Arimamodel<- Arima(train7, model = Arimamodel)
fore7 <- forecast(Arimamodel, h = 119)
rmse7<-sqrt((sum((test7-fore7$mean)^2))*1/119)

data8 = window(data, start=c(1889,8), end=c(1949,7))
train8 = window(data, start=c(1889,8), end=c(1939,8))
test8 = window(data, start=c(1939,9), end=c(1949,7))
Arimamodel<- Arima(train8, model = Arimamodel)
fore8 <- forecast(Arimamodel, h = 119)
rmse8<-sqrt((sum((test8-fore8$mean)^2))*1/119)

data9 = window(data, start=c(1909,9), end=c(1969,8))
train9 = window(data, start=c(1909,9), end=c(1959,9))
test9 = window(data, start=c(1959,10), end=c(1969,8))
Arimamodel<- Arima(train9, model = Arimamodel)
fore9 <- forecast(Arimamodel, h = 119)
rmse9<-sqrt((sum((test9-fore9$mean)^2))*1/119)

data10 = window(data, start=c(1929,10), end=c(1989,9))
train10 = window(data, start=c(1929,10), end=c(1979,10))
test10 = window(data, start=c(1979,11), end=c(1989,9))
Arimamodel<- Arima(train10, model = Arimamodel)
fore10 <- forecast(Arimamodel, h = 119)
rmse10<-sqrt((sum((test10-fore10$mean)^2))*1/119) 
    
data11 = window(data, start=c(1949,11), end=c(2009,11))
train11 = window(data, start=c(1949,11), end=c(1999,11))
test11 = window(data, start=c(1999,12), end=c(2009,10))
Arimamodel <- Arima(train11)
fore11 <- forecast(Arimamodel, h = 119)
rmse11<-sqrt((sum((test11-fore11$mean)^2))*1/119)  
}


{
par(mfrow=c(4,3))

seqplot.ts(data1,fore1$mean, xlab = "Time", ylab = "Value", main = c("Slice01, RMSE", round(rmse1, digits = 1)))

seqplot.ts(data2,fore2$mean, xlab = "Time", ylab = "Value", main = c("Slice02, RMSE", round(rmse2, digits = 1)))

seqplot.ts(data3,fore3$mean, xlab = "Time", ylab = "Value", main = c("Slice03, RMSE", round(rmse3, digits = 1)))

seqplot.ts(data4,fore4$mean, xlab = "Time", ylab = "Value", main = c("Slice04, RMSE", round(rmse4, digits = 1)))

seqplot.ts(data5,fore5$mean, xlab = "Time", ylab = "Value", main = c("Slice05, RMSE", round(rmse5, digits = 1)))

seqplot.ts(data6,fore6$mean, xlab = "Time", ylab = "Value", main = c("Slice06, RMSE", round(rmse6, digits = 1)))

seqplot.ts(data7,fore7$mean, xlab = "Time", ylab = "Value", main = c("Slice07, RMSE", round(rmse7, digits = 1)))

seqplot.ts(data8,fore8$mean, xlab = "Time", ylab = "Value", main = c("Slice08, RMSE", round(rmse8, digits = 1)))

seqplot.ts(data9,fore9$mean, xlab = "Time", ylab = "Value", main = c("Slice09, RMSE", round(rmse9, digits = 1)))

seqplot.ts(data10,fore10$mean, xlab = "Time", ylab = "Value", main = c("Slice10, RMSE", round(rmse10, digits = 1)))

seqplot.ts(data11,fore11$mean, xlab = "Time", ylab = "Value", main = c("Slice11, RMSE", round(rmse11, digits = 1)))
}

rmse_vec<-c(rmse1,rmse2,rmse3,rmse4,rmse5,rmse6,rmse7,rmse8,rmse9,rmse10,rmse11)

avg_RMSE_mean <- mean(rmse_vec)
avg_RMSE_mean
sd_RMSE <- sd(rmse_vec)
sd_RMSE
