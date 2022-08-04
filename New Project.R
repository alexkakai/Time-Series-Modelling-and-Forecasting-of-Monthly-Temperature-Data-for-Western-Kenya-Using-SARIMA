#project
library(tidyverse)
library(zoo)
library(forecast)
library("ggplot2")
library("knitr")
library(fUnitRoots)
library(trend)
library(Hmisc)
library(pastecs)
library(psych)
library(ggfortify)
library(fpp2)
library(MASS)
library(reshape)
library(reshape2)
library(urca)
library("aTSA")
library("tseries")
library(trend)

#1.Dagoretti
dagmin<-read.csv("C:\\Users\\KAKAI\\Dropbox (Personal)\\Project\\data\\dagmin.csv",header = FALSE)
dagmax<-read.csv("C:\\Users\\KAKAI\\Dropbox (Personal)\\Project\\data\\dagmax.csv",header = FALSE)
dagmaxts<-ts(dagmax,frequency = 12,start = c(1980,1))
dagmints<-ts(dagmin,frequency = 12,start = c(1980,1))
ts.plot(dagmaxts,dagmints,col=c("red","blue"),ylab="Temperature (C)",
        main="Plot of Max and Min Temperature of Dagoreti Station")
legend("topright", bty="n", lty=c(1,1), col=c("red","blue"),
       legend=c("Maximum Temp","Minimum Temp"))
abline(reg = lm(dagmints~time(dagmints)))
abline(reg = lm(dagmaxts~time(dagmaxts)))
cycle(dagmaxts)
cycle(dagmints)
plot(aggregate(dagmaxts,FUN = mean),ylab="Temperature (C)",main="Plot of Trendline in Max Temperature of Dagoreti Station")
plot(aggregate(dagmints,FUN = mean),ylab="Temperature (C)",main="Plot of Trendline in Min Temperature of Dagoreti Station")
boxplot(dagmaxts~cycle(dagmaxts),xlab="Months",ylab="Temperature (C)",main="BoxPlot Across Months in Max Temperature of Dagoreti Station")
boxplot(dagmints~cycle(dagmints),xlab="Months",ylab="Temperature (C)",main="BoxPlot Across Months in Min Temperature of Dagoreti Station")



#descriptivestatistics
summary(dagmin)
summary(dagmax)
describe(dagmin)
describe(dagmax)
stat.desc(dagmin)
stat.desc(dagmax)


#ACF/PACF Plots

Acf(dagmaxts, lag.max = 36,type = c("correlation"),plot = TRUE, na.action = na.pass,main = "ACF Plot for Max Temp of Dagoreti")
Pacf(dagmaxts, lag.max = 36,plot = TRUE, na.action = na.pass,main = "PACF Plot for Max Temp of Dagoreti")
Acf(dagmints, lag.max = 36,type = c("correlation"),plot = TRUE, na.action = na.pass,main = "ACF Plot for Min Temp of Dagoreti")
Pacf(dagmints, lag.max = 36,plot = TRUE, na.action = na.pass,main = "PACF Plot for Min Temp of Dagoreti")

#TimeSeriesDecomposition

plot(decompose(dagmints))
plot(decompose(dagmaxts))

#trendcheck
sens.slope(dagmints,conf.level=0.95)
sens.slope(dagmaxts,conf.level = 0.95)

#adftest
adfTest(dagmaxts)
adfTest(dagmints)

#kpsstest
ur.kpss(dagmaxts)
ur.kpss(dagmints)

#TSTransformation

plot(dagmaxts, ylab="Maximum Temperature (C)",main="Monthly Maximum Temperature of Dagoretti(1980-2010)")
sm0<-ma(dagmaxts,order=12,centre = TRUE)
lines(sm0,col="red")
plot(dagmints, ylab="Minimum Temperature (C)",main="Monthly Minimum Temperature of Dagoretti(1980-2010)")
sm1<-ma(dagmints,order=12,centre = TRUE)
lines(sm1,col="blue")


#differencing

#Checkfor1stDifference
ndiffs(dagmaxts)
ndiffs(dagmints)

#CheckforSeasonalDifference
nsdiffs(dagmaxts)
nsdiffs(dagmints)

dagmints %>% diff(lag=12) %>% diff(lag=1) %>% plot(ylab="Temperature (C)",main = "Differenced Minimum Temp of Dagoreti")
dagmaxts %>% diff(lag=12) %>% plot(ylab="Temperature (C)",main = "Differenced Maximum Temp of Dagoreti")

dagmints %>% diff(lag=12) %>% diff(lag=1) %>% Acf(lag.max = 48, main = "Acf Plot for Minimum Temp of Dagoreti")
dagmints %>% diff(lag=12) %>% diff(lag=1) %>% Pacf(lag.max = 48, main = "Pacf Plot for Minimum Temp of Dagoreti")

dagmaxts %>% diff(lag=12) %>% Acf(lag.max = 48, main = "Acf Plot for Maximum Temp of Dagoreti")
dagmaxts %>% diff(lag=12) %>% Pacf(lag.max = 48,main = "Pacf Plot for Maximum Temp of Dagoreti")

dagmints %>% diff(lag=12) %>% Acf(lag.max = 48, main = "Acf Plot for Minimum Temp of Dagoreti")
dagmints %>% diff(lag=12) %>% Pacf(lag.max = 48, main = "Pacf Plot for Minimum Temp of Dagoreti")

#adftest
dagmints %>% diff(lag=12) %>% adfTest()
dagmaxts %>% diff(lag=12) %>% adfTest()

#kpsstest
dagmints %>% diff(lag=12) %>% ur.kpss()
dagmaxts %>% diff(lag=12) %>% ur.kpss()


#Minimum Temperature Model Check

dagmints %>% Arima(order = c(0,1,2),seasonal = c(1,1,2))
dagmints %>% Arima(order = c(0,1,2),seasonal = c(0,1,3))
dagmints %>% Arima(order = c(1,0,2),seasonal = c(1,1,2))
dagmints %>% Arima(order = c(1,0,2),seasonal = c(0,1,3))

#Maximum Temperature Model Check
dagmaxts %>% Arima(order = c(1,0,0),seasonal = c(0,1,1))
dagmaxts %>% Arima(order = c(1,0,0),seasonal = c(3,1,1))

#boxljungtest
min<-dagmints %>% Arima(order = c(1,0,2),seasonal = c(1,1,2))
checkresiduals(min)
min %>% summary()

max<-dagmaxts %>% Arima(order = c(1,0,0),seasonal = c(0,1,1))
checkresiduals(max)
max %>% summary()

Box.test(min$residuals, lag = 24, type = "Ljung-Box")
Box.test(max$residuals, lag = 24, type = "Ljung-Box")

#Diagnostic Testing

min<-dagmints %>% Arima(order = c(1,0,2),seasonal = c(1,1,2))
tsdisplay(resid(min))
qqnorm(resid(min)); qqline(resid(min))

max<-dagmaxts %>% Arima(order = c(1,0,0),seasonal = c(0,1,1))
tsdisplay(resid(max))
qqnorm(resid(max)); qqline(resid(max))
s

dagmaxts %>%
  Arima(order = c(1,0,0),seasonal = c(0,1,1)) %>%
  residuals() %>% ggtsdisplay()

fit3 <- Arima(dagmaxts, order = c(1,0,0),seasonal = c(0,1,1))
checkresiduals(fit3)
fit3 %>% summary()

#Co-Movements
library(lmtest)
grangertest(dagmints, dagmaxts, order = 1)

grangertest(dagmaxts, dagmints, order = 1)
#Focasting

#summary statistics
min %>% summary()
max %>% summary()

fore1 <- predict(min, n.ahead = 132)[1]
fore1

fore2 <- predict(max, n.ahead = 132)[1]
fore2

plot(c(dagmaxts,fore2$pred),type="l",col=c("red","blue"))

ts.plot(dagmaxts,fore2$pred,col=c("black","blue"))
sm0<-ma(fore2$pred,order=12,centre = TRUE)
lines(sm0,col="red")

max %>% forecast::forecast(h=132) %>% autoplot()

forecast::Arima(dagmints,order = c(1,0,2),seasonal = c(1,1,2))


forecast1 = predict(min, n.head = 100)
plot(min)
lines(forecast1$pred, col = 'blue')

fcast <- forecast(min, h=30)
plot(fcast)



fore_arima = forecast::forecast(min, h=24)
df_arima = as.data.frame(fore_arima)
df_arima
dagmints$arima = df_arima$`Point Forecast`
mape(dagmints, dagmints$arima)  ## 2.1%

fore_arimax = forecast::forecast(max, h=24)
df_arimax = as.data.frame(fore_arimax)
df_arimax

fore2 <- predict(max, n.ahead = 24)[1]
fore2
df_arimax1 = as.data.frame(fore2)
df_arimax1

dagmaxts %>%
  Arima(order = c(1,0,0),seasonal = c(0,1,1), lambda=0) %>%
  forecast::forecast(h=20) %>%
  autoplot() +
  ylab("Temperature") + xlab("Year")

dagmints %>%
  Arima(order = c(1,0,2), seasonal = c(1,1,2), lambda=0) %>%
  forecast::forecast(h=20) %>%
  autoplot() + ylab("Temperature") + xlab("Year")

#2.Eldoret
eldmin<-read.csv("C:\\Users\\KAKAI\\Dropbox (Personal)\\Project\\data\\eldmin.csv",header = FALSE)
eldmax<-read.csv("C:\\Users\\KAKAI\\Dropbox (Personal)\\Project\\data\\eldmax.csv",header = FALSE)
eldmaxts<-ts(eldmax,frequency = 12,start = c(1980,1))
eldmints<-ts(eldmin,frequency = 12,start = c(1980,1))
ts.plot(eldmaxts,eldmints,col=c("red","blue"),ylab="Temperature",main="Plot of Max and Min Temperature for Eldoret Station")
legend("topright", bty="n", lty=c(1,1), col=c("red","blue"),
       legend=c("Maximum Temp","Minimum Temp"))
abline(reg = lm(eldmints~time(eldmints)))
abline(reg = lm(eldmaxts~time(eldmaxts)))
cycle(eldmaxts)
cycle(eldmints)
plot(aggregate(eldmaxts,FUN = mean),ylab="Temperature (C)",main="Plot of Trendline in Max Temperature of Eldoret Station")
plot(aggregate(eldmints,FUN = mean),ylab="Temperature (C)",main="Plot of Trendline in Min Temperature of Eldoret Station")
boxplot(eldmaxts~cycle(eldmaxts),xlab="Months",ylab="Temperature (C)",main="BoxPlot Across Months in Max Temperature of Eldoret Station")
boxplot(eldmints~cycle(eldmints),xlab="Months",ylab="Temperature (C)",main="BoxPlot Across Months in Min Temperature of Eldoret Station")

#descriptivestatistics
summary(eldmin)
summary(eldmax)
describe(eldmin)
describe(eldmax)
stat.desc(eldmin)
stat.desc(eldmax)


#ACF/PACF Plots

Acf(eldmaxts, lag.max = 36,type = c("correlation"),plot = TRUE, na.action = na.pass,main = "ACF Plot for Max Temp of Eldoret")
Pacf(eldmaxts, lag.max = 36,plot = TRUE, na.action = na.pass,main = "PACF Plot for Max Temp of Eldoret")
Acf(eldmints, lag.max = 36,type = c("correlation"),plot = TRUE, na.action = na.pass,main = "ACF Plot for Min Temp of Eldoret")
Pacf(eldmints, lag.max = 36,plot = TRUE, na.action = na.pass,main = "PACF Plot for Min Temp of Eldoret")

#TimeSeriesDecomposition

plot(decompose(eldmints))
plot(decompose(eldmaxts))

#trendcheck
sens.slope(eldmints,conf.level=0.95)
sens.slope(eldmaxts,conf.level = 0.95)

#adftest
adfTest(eldmaxts)
adfTest(eldmints)

#kpsstest
ur.kpss(eldmaxts)
ur.kpss(eldmints)

#TSTransformation

plot(eldmaxts, ylab="Maximum Temperature (C)",main="Monthly Maximum Temperature of Eldoret(1980-2010)")
sm0<-ma(eldmaxts,order=12,centre = TRUE)
lines(sm0,col="red")
plot(eldmints, ylab="Minimum Temperature (C)",main="Monthly Minimum Temperature of Eldoret(1980-2010)")
sm1<-ma(eldmints,order=12,centre = TRUE)
lines(sm1,col="blue")


#differencing

#Checkfor1stDifference
ndiffs(eldmaxts)
ndiffs(eldmints)

#CheckforSeasonalDifference
nsdiffs(eldmaxts)
nsdiffs(eldmints)

eldmints %>% diff(lag=12) %>% plot(ylab="Temperature (C)",main = "Differenced Minimum Temp of Eldoret")
eldmaxts %>% diff(lag=12) %>% plot(ylab="Temperature (C)",main = "Differenced Maximum Temp of Eldoret")

eldmints %>% diff(lag=12) %>% Acf(lag.max = 48, main = "Acf Plot for Minimum Temp of Eldoret")
eldmints %>% diff(lag=12) %>% Pacf(lag.max = 48, main = "Pacf Plot for Minimum Temp of Eldoret")

eldmaxts %>% diff(lag=12) %>% Acf(lag.max = 48, main = "Acf Plot for Maximum Temp of Eldoret")
eldmaxts %>% diff(lag=12) %>% Pacf(lag.max = 48,main = "Pacf Plot for Maximum Temp of Eldoret")

#adftest
eldmints %>% diff(lag=12) %>% adfTest()
eldmaxts %>% diff(lag=12) %>% adfTest()

#kpsstest
eldmints %>% diff(lag=12) %>% ur.kpss()
eldmaxts %>% diff(lag=12) %>% ur.kpss()

#Minimum Temperature Model Check

eldmints %>% Arima(order = c(1,0,1),seasonal = c(1,1,1))
eldmints %>% Arima(order = c(1,1,1),seasonal = c(1,1,1))
eldmints %>% Arima(order = c(1,0,1),seasonal = c(0,1,1))
eldmints %>% Arima(order = c(1,1,1),seasonal = c(0,1,1))

#Maximum Temperature Model Check

eldmaxts %>% Arima(order = c(1,0,0),seasonal = c(0,1,1))
eldmaxts %>% Arima(order = c(1,0,0),seasonal = c(0,1,2))
eldmaxts %>% Arima(order = c(1,0,0),seasonal = c(1,1,1))
eldmaxts %>% Arima(order = c(1,0,0),seasonal = c(1,1,2))

#boxljungtest
eldminn<-eldmints %>% Arima(order = c(1,0,1),seasonal = c(0,1,1))
checkresiduals(eldminn)
eldmin %>% summary()

eldmaxx<-eldmaxts %>% Arima(order = c(1,0,0),seasonal = c(0,1,1))
checkresiduals(eldmaxx)
eldmax %>% summary()

Box.test(eldmin$residuals, lag = 24, type = "Ljung-Box")
Box.test(eldmax$residuals, lag = 24, type = "Ljung-Box")

#Forecasting

eldminn %>% summary()
eldmaxx %>% summary()

fore1 <- predict(eldmin, n.ahead = 132)[1]
fore1

fore2 <- predict(eldmax, n.ahead = 132)[1]
fore2


fore_arimax = forecast::forecast(eldminn, h=24)
df_eldmin = as.data.frame(fore_arimax)
df_eldmin


fore_arimax = forecast::forecast(eldmaxx, h=24)
df_eldmax = as.data.frame(fore_arimax)
df_eldmax

df_arimax1 = as.data.frame(fore2)
df_arimax1

eldmaxts %>%
  Arima(order = c(1,0,0),seasonal = c(0,1,1), lambda=0) %>%
  forecast::forecast(h=20) %>%
  autoplot() +
  ylab("Temperature") + xlab("Year")

eldmints %>%
  Arima(order = c(1,0,1),seasonal = c(0,1,1), lambda=0) %>%
  forecast::forecast(h=20) %>%
  autoplot() + ylab("Temperature") + xlab("Year")


#3.Kakamega
kachmin<-read.csv("C:\\Users\\KAKAI\\Dropbox (Personal)\\Project\\data\\kachmin.csv",header = FALSE)
kachmax<-read.csv("C:\\Users\\KAKAI\\Dropbox (Personal)\\Project\\data\\kachmax.csv",header = FALSE)
kachmaxts<-ts(kachmax,frequency = 12,start = c(1980,1))
kachmints<-ts(kachmin,frequency = 12,start = c(1980,1))
ts.plot(kachmaxts,kachmints,col=c("red","blue"),ylab="Temperature",main="Plot of Max and Min Temperature for Kakamega Station")
legend("topright", bty="n", lty=c(1,1), col=c("red","blue"),legend=c("Maximum Temp","Minimum Temp"))
abline(reg = lm(kachmints~time(kachmints)))
abline(reg = lm(kachmaxts~time(kachmaxts)))
cycle(kachmaxts)
cycle(kachmints)
plot(aggregate(kachmaxts,FUN = mean),ylab="Temperature (C)",main="Plot of Trendline in Max Temperature of Kakamega Station")
plot(aggregate(kachmints,FUN = mean),ylab="Temperature (C)",main="Plot of Trendline in Min Temperature of Kakamega Station")
boxplot(kachmaxts~cycle(kachmaxts),xlab="Months",ylab="Temperature (C)",main="BoxPlot Across Months in Max Temperature of Kakamega Station")
boxplot(kachmints~cycle(kachmints),xlab="Months",ylab="Temperature (C)",main="BoxPlot Across Months in Min Temperature of Kakamega Station")

#descriptivestatistics
summary(kachmin)
summary(kachmax)
describe(kachmin)
describe(kachmax)
stat.desc(kachmin)
stat.desc(kachmax)
var(kachmin)
var(kachmax)
sens.slope(kachmints,conf.level=0.95)
sens.slope(kachmaxts,conf.level = 0.95)

#ACF/PACF Plots

Acf(kachmaxts, lag.max = 36,type = c("correlation"),plot = TRUE, na.action = na.pass,main = "ACF Plot for Max Temp of Kakamega")
Pacf(kachmaxts, lag.max = 36,plot = TRUE, na.action = na.pass,main = "PACF Plot for Max Temp of Kakamega")
Acf(kachmints, lag.max = 36,type = c("correlation"),plot = TRUE, na.action = na.pass,main = "ACF Plot for Min Temp of Kakamega")
Pacf(kachmints, lag.max = 36,plot = TRUE, na.action = na.pass,main = "PACF Plot for Min Temp of Kakamega")

#TimeSeriesDecomposition

plot(decompose(kachmints))
plot(decompose(kachmaxts))

#trendcheck
sens.slope(kachmints,conf.level=0.95)
sens.slope(kachmaxts,conf.level = 0.95)

#adftest
adfTest(kachmaxts)
adfTest(kachmints)

#kpsstest
ur.kpss(kachmaxts)
ur.kpss(kachmints)

#TSTransformation

plot(kachmaxts, ylab="Maximum Temperature (C)",main="Monthly Maximum Temperature of Kakamega(1980-2010)")
sm0<-ma(kachmaxts,order=12,centre = TRUE)
lines(sm0,col="red")
plot(kachmints, ylab="Minimum Temperature (C)",main="Monthly Minimum Temperature of Kakamega(1980-2010)")
sm1<-ma(kachmints,order=12,centre = TRUE)
lines(sm1,col="blue")


#differencing

#Checkfor1stDifference
ndiffs(kachmaxts)
ndiffs(kachmints)

#CheckforSeasonalDifference
nsdiffs(kachmaxts)
nsdiffs(kachmints)

kachmints %>% diff(lag=12) %>% diff(lag=1) %>% plot(ylab="Temperature (C)",main = "Differenced Minimum Temp of Kakamega")
kachmaxts %>% diff(lag=12) %>% diff(lag=1) %>% plot(ylab="Temperature (C)",main = "Differenced Maximum Temp of Kakamega")

kachmints %>% diff(lag=12) %>% diff(lag=1) %>% Acf(lag.max = 48, main = "Acf Plot for Minimum Temp of Kakamega")
kachmints %>% diff(lag=12) %>% diff(lag=1) %>% Pacf(lag.max = 48, main = "Pacf Plot for Minimum Temp of Kakamega")

kachmaxts %>% diff(lag=12) %>% diff(lag=1) %>% Acf(lag.max = 48, main = "Acf Plot for Maximum Temp of Kakamega")
kachmaxts %>% diff(lag=12) %>% diff(lag=1) %>% Pacf(lag.max = 48,main = "Pacf Plot for Maximum Temp of Kakamega")

#adftest
kachmints %>% diff(lag=12) %>% diff(lag=1) %>% adfTest()
kachmaxts %>% diff(lag=12) %>% diff(lag=1) %>% adfTest()

#kpsstest
kachmints %>% diff(lag=12) %>% diff(lag=1) %>% ur.kpss()
kachmaxts %>% diff(lag=12) %>% diff(lag=1) %>% ur.kpss()

auto.arima(kachmints)
auto.arima(kachmaxts)
#Minimum Temperature Model Check

kachmints %>% Arima(order = c(2,1,0),seasonal = c(1,1,1))
kachmints %>% Arima(order = c(2,1,0),seasonal = c(0,1,1))
kachmints %>% Arima(order = c(2,0,0),seasonal = c(1,1,1))
kachmints %>% Arima(order = c(2,0,0),seasonal = c(0,1,1))

#Maximum Temperature Model Check

kachmaxts %>% Arima(order = c(1,1,0),seasonal = c(0,1,1))
kachmaxts %>% Arima(order = c(1,1,0),seasonal = c(2,1,1))
kachmaxts %>% Arima(order = c(1,0,0),seasonal = c(0,1,1))
kachmaxts %>% Arima(order = c(1,0,0),seasonal = c(2,1,1))

#boxljungtest
kachminn<-kachmints %>% Arima(order = c(2,0,0),seasonal = c(0,1,1))
checkresiduals(kachminn)
kachminn %>% summary()

kachmaxx<-kachmaxts %>% Arima(order = c(1,0,0),seasonal = c(0,1,1))
checkresiduals(kachmaxx)
kachmaxx %>% summary()

Box.test(kachminn$residuals, lag = 24, type = "Ljung-Box")
Box.test(kachmaxx$residuals, lag = 24, type = "Ljung-Box")

#Forecasting

kachminn %>% summary()
kachmaxx %>% summary()

fore1 <- predict(kachminn, n.ahead = 132)[1]
fore1

fore2 <- predict(kachmaxx, n.ahead = 132)[1]
fore2


fore_arimax = forecast::forecast(kachminn, h=24)
df_kachmin = as.data.frame(fore_arimax)
df_kachmin


fore_arimax = forecast::forecast(kachmaxx, h=24)
df_kachmax = as.data.frame(fore_arimax)
df_kachmax

df_arimax1 = as.data.frame(fore2)
df_arimax1

kachmaxts %>%
  Arima(order = c(1,0,0),seasonal = c(0,1,1), lambda=0) %>%
  forecast::forecast(h=20) %>%
  autoplot() +
  ylab("Temperature") + xlab("Year")

kachmints %>%
  Arima(order = c(2,0,0),seasonal = c(0,1,1), lambda=0) %>%
  forecast::forecast(h=20) %>%
  autoplot() + ylab("Temperature") + xlab("Year")

#4.Nakuru
nakmin<-read.csv("C:\\Users\\KAKAI\\Dropbox (Personal)\\Project\\data\\nakmin.csv",header = FALSE)
nakmax<-read.csv("C:\\Users\\KAKAI\\Dropbox (Personal)\\Project\\data\\nakmax.csv",header = FALSE)
nakmaxts<-ts(nakmax,frequency = 12,start = c(1980,1))
nakmints<-ts(nakmin,frequency = 12,start = c(1980,1))
ts.plot(nakmaxts,nakmints,col=c("red","blue"),ylab="Temperature",main="Plot of Max and Min Temperature for Eldoret Station")
abline(reg = lm(nakmints~time(nakmints)))
abline(reg = lm(nakmaxts~time(nakmaxts)))
cycle(nakmaxts)
cycle(nakmints)
plot(aggregate(nakmaxts,FUN = mean),ylab="Temperature (C)",main="Plot of Trendline in Max Temperature of Eldoret Station")
plot(aggregate(nakmints,FUN = mean),ylab="Temperature (C)",main="Plot of Trendline in Min Temperature of Eldoret Station")
boxplot(nakmaxts~cycle(nakmaxts),xlab="Months",ylab="Temperature (C)",main="BoxPlot Across Months in Max Temperature of Eldoret Station")
boxplot(nakmints~cycle(nakmints),xlab="Months",ylab="Temperature (C)",main="BoxPlot Across Months in Min Temperature of Eldoret Station")

