# Tutorial
# A Little book of R

kings <- scan("http://robjhyndman.com/tsdldata/misc/kings.dat",skip=3)
kingtimeseries <- ts(kings) # time series
plot.ts(kingtimeseries)

# my data
# need date stuff -> stringsAsFactors = FALSE ??
Chf <- read.csv("Data/Swiss_90_sht.csv", header=TRUE)
Chftimeseries <- ts(Chf$Close,frequency=50)
plot.ts(Chftimeseries)
Chftimeseriescomponents <- decompose(Chftimeseries)
plot(Chftimeseriescomponents)
plot(Chftimeseries -Chftimeseriescomponents$seasonal)

births <- scan("http://robjhyndman.com/tsdldata/data/nybirths.dat")

# Jan 1946 to Dec 1959
# freq = 12 cos monthly
# start c(1946,1) = 1 is Jan ...
birthtimeseries <- ts(births, frequency=12, start=c(1946,1))

souvenir <- scan("http://robjhyndman.com/tsdldata/data/fancy.dat")
souvenirtimeseries <- ts(souvenir, frequency=12, start=c(1987,1))

# Plotting
plot.ts(kingtimeseries)
plot.ts(birthtimeseries)
plot.ts(souvenirtimeseries)

# take a log
logsouvenirtimeseries <- log(souvenirtimeseries)
plot.ts(logsouvenirtimeseries)

# Decomposing
# 1. non-seasonal -> consists of trend and irregular
# To estimate the trend component of a non-seasonal time series that can be
# described using an additive model, it is common to use a smoothing method,
# such as calculating the simple moving average of the time series.
library(TTR)
# SMA
kingstimeseriesSMA3 <- SMA(kingtimeseries,n=3)
plot.ts(kingstimeseriesSMA3)

# mmm try SMA 8
kingstimeseriesSMA8 <- SMA(kingtimeseries,n=8)
plot.ts(kingstimeseriesSMA8)

# 2. Seasonal Data
birthstimeseriescomponents <- decompose(birthtimeseries)
souvenirtimeseriescomp <- decompose(souvenirtimeseries)

# the seasonal, trend and irregular components are now stored in variables
# birthstimeseriescomponents$seasonal, birthstimeseriescomponents$trend and
# birthstimeseriescomponents$random.
birthstimeseriescomponents$seasonal # get the estimated values of the seasonal component
plot(birthstimeseriescomponents)
plot(souvenirtimeseriescomp)

# Seasonally Adjusting
# to seasonally adjust the time series of the number of births per month in New
# York city, we can estimate the seasonal component using "decompose()", and
# then subtract the seasonal component from the original time series
birthstimeseriesseasonallyadjusted <- birthtimeseries - birthstimeseriescomponents$seasonal
plot(birthstimeseriesseasonallyadjusted)

Chftimeseriescomponents <- decompose(Chftimeseries)
ChfSeaAdj <- Chftimeseries - Chftimeseriescomponents$seasonal
plot(ChfSeaAdj)
plot(Chftimeseries -Chftimeseriescomponents$seasonal)

# Forecasts using Exponential Smoothing

rain <- scan("http://robjhyndman.com/tsdldata/hurst/precip1.dat",skip=1)
rainseries <- ts(rain,start=c(1813))
plot.ts(rainseries)

rainseriesforecasts <- HoltWinters(rainseries, beta=FALSE, gamma=FALSE)
rainseriesforecasts$fitted
plot(rainseriesforecasts)

Chftimeseriesforecasts <- HoltWinters(Chftimeseries, beta=FALSE, gamma=FALSE)
Chftimeseriesforecasts$fitted
plot(Chftimeseries)
plot(Chftimeseriesforecasts)
tail(Chf)


rainseriesforecasts$SSE

library(forecast)
rainseriesforecasts2 <- forecast.HoltWinters(rainseriesforecasts, h=8)
plot.forecast(rainseriesforecasts2)

Chftimeseriesforecasts2 <- forecast.HoltWinters(Chftimeseriesforecasts, h=8)
plot.forecast(Chftimeseriesforecasts2)

# to calculate a correlogram of the in-sample forecast errors for the London
# rainfall data for lags 1-20, we type:
acf(rainseriesforecasts2$residuals, lag.max=20)

Box.test(rainseriesforecasts2$residuals, lag=20, type="Ljung-Box")

plot.ts(rainseriesforecasts2$residuals)

# -------------------------------------
# Diss
rainfall <- scan("LondonRainfall.txt", skip=1)
rainseries <- ts(rainfall, start=c(1813))
plot.ts(rainseries)
rainseriesforecasts <- HoltWinters(rainseries, beta=FALSE, gamma=FALSE)
rainseriesforecasts <- HoltWinters(rainseries,gamma=FALSE)
rainseriesforecasts <- HoltWinters(rainseries)
plot(rainseriesforecasts)

skirts <- scan("http://robjhyndman.com/tsdldata/roberts/skirts.dat",skip=5)
skirtsseries <- ts(skirts,start=c(1866))
plot.ts(skirtsseries)
skirtsseriesforecasts <- HoltWinters(skirtsseries,gamma=FALSE)
plot(skirtsseriesforecasts)

souvenir <- scan("http://robjhyndman.com/tsdldata/data/fancy.dat")
souvenirtimeseries <- ts(souvenir, frequency=12, start=c(1987,1))
plot(souvenirtimeseries)
souvenirtimeseriesforecasts <- HoltWinters(souvenirtimeseries)
plot(souvenirtimeseriesforecasts)

# auto correl
apseries <- ts(AirPassengers, frequency=12, start=c(1949))
plot.ts(apseries)
aa <- acf(apseries,lag.max=30)
plot.ts(aa)
aa <- acf(apseries,lag.max=80)
plot.ts(aa)
apseriescomp <- decompose(apseries)
bb <- acf(apseriescomp$seasonal,lag.max=20)
plot.ts(bb)

# Exp smoothing ...
# use AirPassengers built in set
plot.ts(BJsales)
hwforecast <- HoltWinters(BJsales,beta=FALSE, gamma=FALSE)
hwforecast$fitted
plot(hwforecast)
hwforecast$SSE #SSE error
#forecasting
libray(forecast)

# acf, pacf ...
kings <- scan("http://robjhyndman.com/tsdldata/misc/kings.dat",skip=3)
kingtimeseries <- ts(kings) # time series
kingtimeseriesdiff1 <- diff(kingtimeseries, differences=1)
#plot.ts(kingtimeseriesdiff1)
acf(kingtimeseriesdiff1, lag.max=20, main='Auto-correlation') # plot a correlogram
#acf(kingtimeseriesdiff1, lag.max=20, plot=FALSE) # get the autocorrelation values
#Autocorrelations of series 'kingtimeseriesdiff1', by lag

pacf(kingtimeseriesdiff1, lag.max=20, main='Partial Auto-correlation') 
