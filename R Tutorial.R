setwd("D:/RWorkingDir")

# -------------- Reading Data ------------------------
# read.table() command
indata <- read.table("clipboard")
mydata <- read.table("C:/WINDOWS/Desktop/blah.dat",header=TRUE)

indata <- read.table("clipboard")
indata
indata[,1]
indata[,2]
indata[1,]
ind_av <- mean(indata[,1])
ind_1 <- indata[,1]
barplot(ind_1)
abline(h=mean(indata[,1]))
dev <- sd(indata[,1])
abline(h=mean(indata[,1]) + dev)
abline(h=mean(indata[,1]) - dev)
str(indata)

Opening a Data File
mydata <- read.table("C:/WINDOWS/Desktop/blah.dat",header=TRUE)
csvdataH <- read.csv("Swiss_90.csv",header=TRUE)

# listing data sets
data()
library(help="datasets")
str(DNase)
#Time Series - AirPassengers, BJsales, BJsales.lead
#DF - BOD, 
#?? - CO2, ChickWeight, DNase

# Now mydata is a dataframe with named columns, ready for analysis. Note that R assumes that there are
# no labels on the columns, and gives them default values, if you omit the header=TRUE argument. Now let's
# suppose that instead of blah.dat we have blah.dta, a stata le.
# library(foreign)
# mydata <- read.dta("C:/WINDOWS/Desktop/blah.dta")

# Another data format we may read is .csv comma-delimited les (such as those
# exported by spreadsheets). These les are very similar to those mentioned
# above, but use punctuation to delimit columns and rows. Instead of
# read.table(), we use read.csv(). Fixed width les can be read using read.fwf()

csvdata <- read.csv("Swiss_90.csv")
csvdata

csvdataH <- read.csv("Swiss_90.csv",header=TRUE)
csvdataH
head(csvdataH)
str(csvdataH)
sum5 = filter(csvdataH[,5], rep(1, 5), sides = 1)
sum5
ma_sum5 = sum5/5
head(ma_sum5)

as_sma <- function(x,ma){
  + return( ((filter(x, rep(1, ma), sides = 1))/ma) )
}

ma_col <- as_sma(csvdataH[,5],5)
ma_col

csvdataHts <- ts(csvdataH[,2], frequency=10)
csvdataHts
plot.ts(csvdataHts)
csvdataHtscomp <- decompose(csvdataHts)
plot(csvdataHtscomp)
plot(csvdataHtscomp$trend)



# csvdataT <- read.table("Swiss_90.csv",header=TRUE) # ERROR!!!


# --------- Play
NikDax <- read.table("clipboard", header=TRUE)
NikDax
nik <- NikDax[,2]
nik
Dax <- NikDax[,3]
Dax
dz_lm <- lm(Dax~nik)
max(Dax)
which.max(Dax)
dz_lm
summary(dz_lm)

diffDax <- diff(Dax)
diffDax
Dax
diffDax <- diff(Dax, lag=1, difference =2)
diffDax
lagDax <- lag(Dax, k=2)
lagDax

Daxts <- ts(Dax, frequency=50)
plot.ts(Daxts)

ma <- function(Daxts,n=5){filter(Daxts,rep(1/n,n), sides=2)}
ma
help(rep)

library(zoo)
zdata <- NikDax[,3] # Dax
zindex <- NikDax[,1] # Date
zdata
zindex
zDax <- zoo(zdata,zindex)
zDax
zDaxmean <- rollmean(zDax, 5, fill = NA)
zDaxmean

#filtering
x <- 1:100
x
filter(x, rep(1, 3))
j = filter(x, rep(1, 3), sides = 1)
j
maj = j/3
maj

# built in data sets
data() #lists them
data(AirPassengers)
AirPassengers
BJsales
cars
head(cars)

# -----------------------------------
indata <- read.table("clipboard")
indata
plot(indata$V5, type="l")
?par()
library("ggplot2")
ggplot(indata, aes(x=indata$V1,y=indata$V5)) + geom_line()

dx <- read.table(file.choose(), header=T)
cs <- read.csv("data/Dax_90.csv",header=TRUE)
head(cs)
y <- cs[['Open']]
str(y)

str(Titanic)
length(Titanic)
Titanic[]