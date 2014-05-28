crime <- scan("ArmedRobbery.txt")
crime
crimetimeseries <- ts(crime, frequency=12, start=c(2000,1))
crimetimeseries
plot.ts(crimetimeseries)

# Decompose a timeseries 
crimecomponents <- decompose(crimetimeseries)
plot(crimecomponents)
# view one of the components . . 
crimecomponents$trend
plot(crimecomponents$trend)

f100 <- scan("F1002013.txt")
f100
f100ts <- ts(f100, frequency=250, start=c(2013,1, 1), end=c(2013,12,31))
f100ts <- ts(f100, start=c(2013,1, 1), end=c(2013,12,31), frequency=250)
f100ts <- ts(f100, frequency=50)
f100ts <- ts(f100)
plot.ts(f100ts)
f100tscomponents <- decompose(f100ts)
plot(f100tscomponents)

f100_2 <- scan("F1002013_2.txt"$V2)

# exponential smoothing
rainfall <- scan("LondonRainfall.txt", skip=1)
rainseries <-ts(rainfall, start=c(1813))
plot.ts(rainseries)
rainseriesSmoothing <- HoltWinters(rainseries, alpha=0.4, beta=FALSE, gamma=FALSE)
plot(rainseriesSmoothing)
rainseriesForecast <- forecast.HoltWinters(rainseriesSmoothing , h=8)
plot(rainseriesForecast)



# autocorrelation and correlogram

acf(crimetimeseries, lag.max=20)
acf(crimetimeseries, lag.max=120)
acf(crimecomponents$seasonal)
#note: default for lag.max in 10*log10(N)

# Commands for labwork:

# Rainfall Exercise (as per commands above in the lecture notes)
rainfall <- scan("LondonRainfall.txt", skip=1)
rainfallts <- ts(rainfall, start=c(1813))
plot.ts(rainfallts)
rainseriesforecasts <- HoltWinters(rainfallts, beta=FALSE, gamma=FALSE)
rainseriesforecasts2 <- forecast.HoltWinters(rainseriesforecasts, h=8)
plot(rainseriesforecasts2)


# births Exercise
births <- scan("nybirths.txt")
births_ts <-ts(births, frequency=12, start=c(1946))
plot(births_ts)
birth_components <- decompose(births_ts)
plot(birth_components)
acf(births_ts)


# souvenir Exercise
souv <- scan("souvenir.txt")
souv_ts <- ts(souv, frequency=12, start=c(1987,1))
plot.ts(souv_ts)
souv_components <- decompose(souv_ts)
plot(souv_components)
acf(souv_ts, lag=40)

# --- Stuar51XT -----------
data()
data(BOD)
class(BOD)
head(BOD)
str(BOD)
head(AirPassengers)
str(AirPassengers)
data(package="datasets")
data(Titanic)
head(CO2)
class(CO2)
str(CO2)
CO2[order(CO2$conc),] # remeber order by rows so , after ...
attach(CO2)
CO2[order(conc),] # now attached
CO2[order(conc,uptake),]
CO2[conc > 400,]
CO2[Type=="Quebec",] # REMEMBER => want ROWS, so COMMA after ...
CO2[Type=="Quebec" & conc > 400,]

CO2[sapply(CO2,is.numeric)] # ABSENSE of comma == COLUMNS ...
CO2[sapply(CO2,is.factor)]

names(CO2)
