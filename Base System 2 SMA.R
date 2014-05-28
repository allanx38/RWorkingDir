
library(TTR)
Dax <- read.csv("Data//Dax_2000.csv",header=TRUE)
#Dax <- read.csv("Data//Dow_2000.csv",header=TRUE)
#Dax <- read.csv("Data//N225_2000.csv",header=TRUE)
#Dax <- read.csv("Data//F100_2000.csv",header=TRUE)
#Dax <- read.csv("Data//SP500_2000.csv",header=TRUE)
#Dax <- read.csv("Data//Oz_2000.csv",header=TRUE)

BaseSys2_SMA <- function(file, sma, mkt, SLoss) {
  Dax <- read.csv(file,header=TRUE) #read file
  sma_per <- sma                    #read sma value
  sma <- SMA(Dax["Open"], sma_per)  #create sma vector
  Dax <- cbind(Dax, sma)            #add sma vector as new col
  Dax$Long <- ifelse(Dax$Open > Dax$sma, Dax$Close - Dax$Open, NA) #if O > SMA add to Long col
  Dax$Short <- ifelse(Dax$Open < Dax$sma, Dax$Open-Dax$Close, NA)  #if O < SMA add to Long col
  print(c(mkt, " - Long PL SMA", sma_per, round(sum(Dax$Long, na.rm=TRUE))))   #print Long res
  print(c(mkt, " - Short PL SMA", sma_per, round(sum(Dax$Short, na.rm=TRUE)))) #print Short res
  print(c("Long PL per trade" ,round(sum(Dax$Long, na.rm=TRUE) / sum(!is.na(Dax$Long)),2 )))
  # Long Winners
  print(c("Long Wins %",round(sum(!is.na(Dax$Long[Dax$Long > 0])) / sum(!is.na(Dax$Long)) * 100) ))
  # Short pl per trade
  print(c("Short PL per trade", round(sum(Dax$Short, na.rm=TRUE) / sum(!is.na(Dax$Short)),2) ))
  # Short Winners 
  print( c("Short Wins %", round(sum(!is.na(Dax$Short[Dax$Short > 0])) / sum(!is.na(Dax$Short)) * 100) ) )
  print(summary(Dax[c(7,8)]))
  #SLoss
  Dax$Long <- ifelse(!is.na(Dax$Long), ifelse(Dax$Long < SLoss, SLoss, Dax$Long), NA)
  lg_pl <- round(sum(Dax$Long, na.rm=TRUE))
  print(c("Long S Loss",lg_pl, "Year", round(lg_pl/13)))
  Dax$Short <- ifelse(!is.na(Dax$Short), ifelse(Dax$Short < SLoss, SLoss, Dax$Short), NA)
  lg_pl <- round(sum(Dax$Short, na.rm=TRUE))
  print(c("Short S Loss",lg_pl, "Year", round(lg_pl/13)))
}

BaseSys2_SMA("Data//CommonDate//Dow_2000_d.csv", 200, "Dow", -100)
BaseSys2_SMA("Data//CommonDate//Dax_2000_d.csv", 25, "Dax")
BaseSys2_SMA("Data//CommonDate//F100_2000_d.csv", 5, "FTSE")
BaseSys2_SMA("Data//CommonDate//CAC_2000_d.csv", 200, "CAC")
BaseSys2_SMA("Data//CommonDate//N225_2000_d.csv", 5, "N225", -200)
BaseSys2_SMA("Data//CommonDate//Oz_2000_d.csv", 25, "Oz")
BaseSys2_SMA("Data//CommonDate//Gold_d.csv", 25, "Gold")
BaseSys2_SMA("Data//CommonDate//Silver_d.csv", 25, "Silver")
BaseSys2_SMA("Data//CommonDate//GBUS_d.csv", 25, "N225")

Dax <- read.csv("Data//CommonDate//Dow_2000_d.csv",header=TRUE)
sma_per <- 25
sma <- SMA(Dax["Open"], sma_per)
Dax <- cbind(Dax, sma)
tail(Dax)

Dax$Long <- ifelse(Dax$Open > Dax$sma, Dax$Close - Dax$Open, NA)
Dax$Short <- ifelse(Dax$Open < Dax$sma, Dax$Open-Dax$Close, NA)

# pl
c("Long PL SMA", sma_per, round(sum(Dax$Long, na.rm=TRUE),4))
c("Short PL SMA", sma_per, round(sum(Dax$Short, na.rm=TRUE)))

write.csv(Dax,"Data//Dow_2000_sma.csv")

#sum(Dax$Long, na.rm=TRUE)
#sum(Dax$Short, na.rm=TRUE)

# Long pl per trade
sum(Dax$Long, na.rm=TRUE) / sum(!is.na(Dax$Long)) 

# Long Winners
round(sum(!is.na(Dax$Long[Dax$Long > 0])) / sum(!is.na(Dax$Long)) * 100) 

# Short pl per trade
sum(Dax$Short, na.rm=TRUE) / sum(!is.na(Dax$Short)) 

# Short Winners
round(sum(!is.na(Dax$Short[Dax$Short > 0])) / sum(!is.na(Dax$Short)) * 100) 

summary(Dax[c(7,8)])
summary(Dax)


# ----------------------------------------

Mkt <- read.csv("Data//CommonDate//Dow_2000_d.csv",header=TRUE)

tail(Mkt)
Mkt$pl <- lapply(Mkt$pl, OpCl, OC=Mkt$Close-Mkt$Open, OL=Mkt$Open-Mkt$Low, SLoss = -100))
Mkt$pl <- sapply(Mkt$Close, function(x) x * 10)
Mkt$pl <- sapply(Mkt$Close, OpCl, OC=100, OL=50, SLoss=70)

sapply(Mkt[-1],mean)
apply(Mkt,2,is.na)
apply(apply(Mkt,2,is.na) ,2, sum) #counts the na's in each col

sapply(Mkt[-1],function(x) x + 5)

Mkt$pl <- sapply(Mkt$Close, OpCl, OC=Mkt$Close-Mkt$Open , OL=Mkt$Low-Mkt$Open, SLoss=-50)
tail(Mkt$pl)
warnings()

OpCl <- function(x,OC=200, OL=50, SLoss=70){
  #browser()
  if (OL < SLoss){
    return(OL)
  } else {
    return(OC)
  }
}

Mkt <- read.csv("Data//CommonDate//Dax_2000_d.csv",header=TRUE)
Mkt <- read.csv("Data//CommonDate//N225_2000_d.csv",header=TRUE)
z <- length(Mkt[,1])
for (i in 1:z){
  SLoss <- -100
  if(Mkt$Low[i]-Mkt$Open[i] < SLoss){
    pl <- SLoss
  } else {
    pl <- (Mkt$Close[i]-Mkt$Open[i])
  }
  Mkt$pl[i] <- pl
}
tail(Mkt$pl)
sum(Mkt$pl)

Mkt$LngPL <- 0
fn1 <- function(Mkt, SLoss){
  pl <- 0
  len <- length(Mkt[,1])
  for (i in 1:len){
    if(Mkt$Low[i]-Mkt$Open[i] < SLoss){
      pl <- pl + SLoss
    } else {
      pl <- pl + Mkt$Close[i]-Mkt$Open[i]
    }
  }
  return(pl)
}
fn1(Mkt, -40)
tail(Mkt)


# -------- Prev Winners ------------
Dax$prevHigh <- c( "NA", Dax$High[ - length(Dax$High) ] )

DLP2 <- ifelse(Dax$DLP > 0, Dax$Long, NA)
Dax$DLP <- c(NA, Dax$Long[-length(Dax$Long)])
sum(DLP2, na.rm=TRUE)
tail(DLP, n=20); 
tail(Dax$Long, n=20)
tail(Dax[c(9,11)])



# -------------- zoo ----------------
library(xts)
zz <- read.zoo("Data//Dow_2000.csv", sep = ",",format="%d-%b-%y",  header=TRUE)
zz <- read.zoo("Data//Dax_2000.csv", sep = ",",format="%d/%m/%Y",  header=TRUE)
zz <- read.zoo("Data//N225_2000.csv", sep = ",",format="%d/%m/%Y",  header=TRUE)

tail(zz)
xx<- as.xts(zz)
tail(xx)
x<-xx['2013']
tail(x)
x$OC <- x$Close - x$Open
x$OC_P <- c(NA, x$OC[-length(x$OC)])
sum(ifelse(x$OC_P > 0, x$OC, NA),na.rm=TRUE)

# --------- Prev Day and SMA ------------------------
