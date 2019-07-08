#Installera Packages
# setwd("C:/Users/Shadman/Desktop/TidserieAnalys/Projct1")
install.packages(c("readxl","stats","forecast", "aTSA"))

#L?sa in packages
library(readxl)
library(stats)
library(aTSA)
library(forecast)
library(smooth)
library(tseries)
library(fUnitRoots)
library(TSA)

#PreProcessing
data <- read_excel("Data.xlsx", col_types = c("date", "numeric"))
data.ts <- ts(data$Milk.Prod, start=1995, freq=12)
plot(data.ts, xlab = "Year" ,ylab = "Milk Production")
plot(decompose(data.ts))

#68/32
train <- data[1:156,]
train.ts <- ts(train$Milk.Prod, start=1995, freq=12)
test <- data[157:228,]
test.ts <- ts(test$Milk.Prod, start=2008, freq=12)

#test if series is statinary 
d<-ndiffs(data.ts)
train.ts = diff(train.ts, differences=d)
ndiffs(train.ts)

#Decomposition
train.decomp <- stl(train.ts, s.window = "periodic", t.window = 0.5*length(train.ts))
plot(train.decomp)

#Remainder
train.ts.remainder <- train.decomp$time.series[,'remainder']
acf(train.ts.remainder, main = "ACF of Remainder") #p = 5 from lag
pacf(train.ts.remainder, main = "PACF of Remainder")


##########################################################

#Potential p = 1, 3,5,7 
#Potential q = " 1", "2"," 3"," 4"," 5"," 12"," 13"," 14"," 15"," 16"," 17"," 18"," 19"

{
  P1 = c()
  P1$AIC = c()
  P1$RMSE = c()
  
  i = 1
  while (i<=19) {
    ArimaModel <- arima(train.ts.remainder, c(1,0,i))
    model.test <- Arima(test.ts, model = ArimaModel)
    P1$RMSE[i] <- accuracy(model.test)[2]
    P1$AIC[i] <- model.test$aic
    i = i + 1
  }
  P1 <- data.frame(P1)
  
  P5 = c()
  P5$AIC = c()
  P5$RMSE = c()
  
  i = 1
  while (i<=19) {
    ArimaModel <- arima(train.ts.remainder, c(3,0,i))
    model.test <- Arima(test.ts, model = ArimaModel)
    P5$RMSE[i] <- accuracy(model.test)[2]
    P5$AIC[i] <- model.test$aic
    i = i + 1
  }
  P5 <- data.frame(P5)
  
  P7 = c()
  P7$AIC = c()
  P7$RMSE = c()
  
  i = 1
  while (i<=19) {
    ArimaModel <- arima(train.ts.remainder, c(5,0,i))
    model.test <- Arima(test.ts, model = ArimaModel)
    P7$RMSE[i] <- accuracy(model.test)[2]
    P7$AIC[i] <- model.test$aic
    i = i + 1
  }
  P7 <- data.frame(P7)
  
  P11 = c()
  P11$AIC = c()
  P11$RMSE = c()
  
  i = 1
  while (i<=19) {
    ArimaModel <- arima(train.ts.remainder, c(7,0,i))
    model.test <- Arima(test.ts, model = ArimaModel)
    P11$RMSE[i] <- accuracy(model.test)[2]
    P11$AIC[i] <- model.test$aic
    i = i + 1
  }
  P11 <- data.frame(P11)
}

##############################################

#Arima Auto
ArimaModel <- arima(train.ts.remainder, c(5,0,18))
summary(ArimaModel)
acf(ArimaModel$residuals)
pacf(ArimaModel$residuals)

ArimaModel <- arima(train.ts.remainder, c(5,1,15))
acf(ArimaModel$residuals)
pacf(ArimaModel$residuals)

#Test model on test data
model.test <- Arima(test.ts, model = ArimaModel)
accuracy(model.test)
modelresidu <-ArimaModel$residuals
h<-hist(modelresidu, xlab="Residuals",
     main="Histogram of residuals", prob = TRUE)

lines(density(modelresidu), col="blue", lwd=2)  

#Plot predicted vs actual
{
  plot.ts(model.test$fitted, type = "l", col = "green", xlab = "Time", ylab = "Milk Production")
  par(new=TRUE)
  plot.ts(test.ts, type = "l", col = "blue", bty ="n",axes=F,frame.plot=F, xaxt='n', ann=FALSE, yaxt='n')
}

P1 <- P1[-c(6, 7, 8, 9, 10, 11), ]
P5 <- P5[-c(6, 7, 8, 9, 10, 11), ]
P7 <- P7[-c(6, 7, 8, 9, 10, 11), ]
P11 <- P11[-c(6, 7, 8, 9, 10, 11), ]




# library
library(ggplot2)

# create a dataset for RMSE
q_value=rep(c(" 1", "2"," 3"," 4"," 5"," 12"," 13"," 14"," 15"," 16"," 17"," 18"," 19"),3)
p_value=c(rep(" 1" , 13), rep( " 5" ,13), rep( "7",13))
RMSE <- c(P1$RMSE, P5$RMSE, P7$RMSE)
data1=data.frame(q_value,p_value,RMSE)


# Stacked blot RMSE
ggplot(data1, aes(fill=p_value, y=RMSE, x=q_value)) +
  geom_bar( position="dodge", stat="identity")

# create a dataset for AIC
q_value=rep(c(" 1", "2"," 3"," 4"," 5"," 12"," 13"," 14"," 15"," 16"," 17"," 18"," 19"),3)
p_value=c(rep(" 1" , 13), rep( " 5" ,13), rep( "7",13))
AICs <- c(P1$AIC, P5$AIC, P7$AIC)
data=data.frame(q_value,p_value,AICs)


# Stacked blot AIC
ggplot(data, aes(fill=p_value, y=AICs, x=q_value)) +
  geom_bar( position="dodge", stat="identity")
