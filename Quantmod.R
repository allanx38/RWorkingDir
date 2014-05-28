# quantmod

library(quantmod)
library(xts)
library(TTR)
library(Rcmdr)

# youtube video ...
getSymbols("IBM",from=Sys.Date()-365)
IBM
str(IBM)
colnames(IBM)
rownames(IBM)
IBMDF <- data.frame(IBM)
head(IBMDF)
IBMDF$date <- as.Date(rownames(IBMDF))
ibm <- IBMDF[ -c(1,2,3,5,6)]
head(ibm)

getSymbols("ORCL", from=Sys.Date()-365)
ORCL
head(ORCL)
ORCLDF <- data.frame(ORCL)
head(ORCLDF)
ORCLDF$date <- as.Date(rownames(ORCLDF))

orcl <- ORCLDF[ -c(1,2,3,5,6)]
head(orcl)

pair <- merge(ibm,orcl, by="date")
head(pair)
pair$diff <- pair$IBM.Close - pair$ORCL.Close

getSymbols("^DJI", src="yahoo", from=Sys.Date()-365)
getSymbols("^GDAXI", src="yahoo", from=Sys.Date()-365)
head(GDAXI)
GDAXI$date <- as.Date(rownames(GDAXI))
GDAXIDF <- data.frame(GDAXI)
dax <- GDAXIDF[ -c(1,2,3,5)]
tail(dax)
getSymbols("DJIA", src='FRED',  from=Sys.Date()-365)
head(DJIA)
tail(DJIA)

dxdow <- merge(DJIA,dax,by="date")
tail(dxdow)

# ----------------------------------
Dax = read.csv("D:/Allan/DropBox/MSc/Dissertation/Thesis/Data/Dax_2000_d.csv")
head(Dax)
x <- xts(Dax[,c(2:5)],Dax[,1])
Dx = as.xts(read.zoo("D:/Allan/DropBox/MSc/Dissertation/Thesis/Data/Dax_2000_d.csv",header=T))
class(Dx)
head(Dx)
chartSeries(Dx)

read.xts <- function(x, format = "%d/%m/%Y", header = TRUE, sep = ",") {
  result <- as.xts(read.zoo(x, sep = sep, format = format, header = header))
  return(result)
}
#Dx <- read.xts(file.choose())
Dx <- read.xts("D:/Allan/DropBox/MSc/Dissertation/Thesis/Data/Dax_2000_d.csv")

chartSeries(Dx,TA=NULL,name='Allan', theme='white',subset='2013')


?chartSeries

autoplot.zoo(Dx$Close) + ggtitle("Dax 2000-2013")

ggplot( Dax$Close, aes(date) ) + labs( title = "S&P 500 (ggplot2::ggplot)")

ggplot(Dax, aes(Date, Close)) 

+ geom_line() +
  scale_x_date(format = "%b-%Y") + xlab("") + ylab("Daily Views")

  # ======================================

# moving average
mav <- function(x,n=5){filter(x,rep(1/n,n), sides=2)}
dax$ma10 <- mav(dax$ORCL.Close,10)
tail(dax,n=15)

keeps <- c(ORCLDF$date,ORCLDF$IBM.Close)
head(keeps)
ORCLDF[,keeps,drop=FALSE]

DF <- data.frame(
  x=1:10,
  y=10:1,
  z=rep(5,10),
  a=11:20
)
drops <- c("x","z")
df[ -c(1,3:6, 12) ]

getwd()
# load from csv ...
dataFile_strip <- "Dax_90.csv"
dataDir <- "Data"
file2 <- paste(dataDir,"/",dataFile_strip,sep="")
zz <- read.zoo(file2, sep = ",",format="%d/%m/%Y", header=TRUE,index.column=1,colClasses=c("character",rep("numeric",5)))

zz <- read.zoo(file2, sep = ",",format="%d/%m/%Y", header=TRUE)

head(zz)
xx<- as.xts(zz)

x<-xx['2013']
barChart(x)
#addVo()
addTA(EMA(Vo(x),n=20))
OpHi(x)
LoCl(x)
length(x$Low)
length(x$Close)
head(x)
# getSymbols("Dax",src="scv")


# load from google
getSymbols("SPY",src="google",from="2010-01-01")
head(SPY)
length(SPY)
is.xts(SPY)
SPY.CLose <- SPY[,"Close"]
head(SPY.CLose)
is.xts(SPY.CLose)

# charting
chartSeries(SPY, type="line",subset="last 3 weeks",theme=chartTheme('white'))
args(chartSeries)


Op(SPY) #quantmod
seriesHi(SPY)

sp <- tail(SPY)
OpCl(sp)
Cl(sp) - Lag(Cl(sp))
Lag(Cl(sp))

sp13 <- SPY['2013']
length(sp13[,1])
if (Cl(sp13) > Op(sp13)) as <- (Op(sp13) - Cl(sp13)) else as <- 0
length(as)

if (Cl(sp13) > Lag(Cl(sp13)) aa <- (Cl(sp13) - Lag(Cl(sp13)) else aa <- 0
if (Cl(sp13) > Lag(Cl(sp13))) aa <- 2 aa <- 1
Lag(Cl(sp13))
sp13.c <- Cl(sp13)
sp13.c[,"prevCl"] <- Lag(Cl(sp13))
ab <- Lag(Cl(sp13))
sp13.c[,"prevCl"] <- ab
length(sp13.c)
length(ab)

# ------------- R Dummies
# checks input and does logit calculation 
logit <- function(x){ 
  x <- ifelse(x < 0 | x> 1, NA, x) 
  #browser()
  log(x / (1-x))
}

# transforms percentage to number and calls logit 
logitpercent <- function(x){
  x <- gsub("%","",x)
  logit(as.numeric(x))
}
logitpercent("5%")
traceback()
debug(logit)


# Chp 9
# if stsaments
# if else
# vectorising choices
# switching
#Looping
# Apply family - 7 functions

#count birds inn garden ...
counts <- matrix(c(3,2,4,6,5,1,8,6,1), ncol=3)
colnames(counts) <- c("sparrow", "dove", "crow")
counts
#Each column represents a different species, 
# and each row represents a different day
# 1. calc max count per specis on any day
apply(counts, 2, max)
# 1 = rows, 2 = cols, no paraenthesis needed after fnc name
# apply splits matrix into vectors, so fnc needs to be able to deal with vectors

chf <- read.csv("Swiss_90.csv",header=TRUE)
head(chf[-1]) #ignoring col 1
apply(chf[-c(1,2)],2,mean) #max no Date col
chf$newCol <- apply(chf, 2, function(x) chf$Open - chf$Close)
chf$newCol <- chf$Open - chf$Close

#sapply - s = simple
# 2 args 1. data set, 2. fnc NOTE no 1,2 = works on elements ...

#lapply -> returns lists ...

# -----------------------
chf <- read.csv("Swiss_90.csv",header=TRUE)
head(chf)

num <- 1:30*2
num
num[3]
num[-head(num)]

chf$Date
length(chf)
chf[,"ncol"] <- NA
chf$ncol2 <- NA
chf$ncol3 <- 3
chf$nol <- chf$Open - chf$Close
sum(chf$nol)

# ---------------------------
str(Titanic)
library(datasets)
library(help="datasets")
data(volcano)
data(Titanic)
apply(Titanic,1,sum) 
apply(Titanic,2,sum)
apply(Titanic,3,sum)
apply(Titanic,4,sum)
apply(Titanic,c(2,4),sum)

# sapply on lists ...
iris
sapply(iris,mean)
sapply(iris,function(x) ifelse(is.numeric(x),mean(x),NA))
sapply(iris,function(x) mean(x))

str(mtcars)
cars <- within(mtcars,am <- factor(am,levels=0:1,labels=c("Auto","Manual")))
str(cars)
with(cars, tapply(mpg,am,mean))

# ggplot2 ----------------------------
library(ggplot2)
str(mpg)
#first plot
qplot(displ, hwy, data = mpg)
qplot(displ, hwy, data = mpg, color = drv) #colour coded
qplot(displ, hwy, data = mpg, geom = c("point","smooth"))
qplot(hwy, data= mpg, fill = drv) #hist
qplot(displ, hwy, data= mpg, facets = .~drv) # split into 3 scatter .~drv is rows~cols, . is no extra rows
qplot(hwy, data= mpg, facets = drv~., binwidth=2) #spli 3 hist

chf <- read.csv("Swiss_90_sht.csv",header=TRUE)
head(chf)
qplot(chf$Date,chf$Close,color=chf$High)

g <- ggplot(chf, aes(chf$Date, chf$Close))
summary(g)
print(g) # ERROR
p <- g + geom_point()
print(p)
g + geom_point() # auto-printing!!!
g + geom_point()
g + geom_point() + geom_smooth()


