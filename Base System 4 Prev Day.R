
# ---------- Reversing Prev Day ... --------------------
Dax <- read.csv("Data//Dax_2000.csv",header=TRUE)
Dax <- read.csv("Data//Dow_2000.csv",header=TRUE)
Dax <- read.csv("Data//N225_2000.csv",header=TRUE)
Dax <- read.csv("Data//F100_2000.csv",header=TRUE)
Dax <- read.csv("Data//CAC_2000.csv",header=TRUE)
#Dax <- read.csv("Data//SP500_2000.csv",header=TRUE)
#Dax <- read.csv("Data//Oz_2000.csv",header=TRUE)

Dax$OC <- Dax$Close - Dax$Open
Dax$OC_P <- c(NA, Dax$OC[-length(Dax$OC)])
sum(ifelse(Dax$OC_P > 0, Dax$OC, NA),na.rm=TRUE)
DDD <- ifelse(Dax$OC_P > 0, Dax$OC, NA)
sum(DDD, na.rm=TRUE)
tail(Dax)
tail(Dax$OC_P)

prevS <- function(){
  fil <- c("Data//Dow_2000.csv","Data//SP500_2000.csv","Data//Dax_2000.csv","Data//CAC_2000.csv","Data//F100_2000.csv","Data//N225_2000.csv","Data//Oz_2000.csv")
  for (i in 1:length(fil)){
    Dax <- read.csv(fil[i], header=TRUE)
    Dax$OC <- Dax$Close - Dax$Open
    Dax$OC_P <- c(NA, Dax$OC[-length(Dax$OC)])
    print(fil[i])
    print(sum(ifelse(Dax$OC_P < 0, Dax$OC, NA),na.rm=TRUE))
    print(sum(ifelse(Dax$OC_P < 0, Dax$OC, NA),na.rm=TRUE)/13)
  }
}
prevS()

prevL <- function(){
  fil <- c("Data//Dow_2000.csv","Data//SP500_2000.csv","Data//Dax_2000.csv","Data//CAC_2000.csv","Data//F100_2000.csv","Data//N225_2000.csv","Data//Oz_2000.csv")
  for (i in 1:length(fil)){
    Dax <- read.csv(fil[i], header=TRUE)
    Dax$OC <- Dax$Close - Dax$Open
    Dax$OC_P <- c(NA, Dax$OC[-length(Dax$OC)])
    print(fil[i])
    print(sum(ifelse(Dax$OC_P > 0, Dax$OC, NA),na.rm=TRUE))
    print(sum(ifelse(Dax$OC_P > 0, Dax$OC, NA),na.rm=TRUE)/13)
  }
}
prevL()


# ---------- Multiple Systems -----------------

SMA_PrevD <- function(file, sma, mkt) {
  Dax <- read.csv(file,header=TRUE) #read file
  
  # calc and add sma col
  sma_per <- sma                    #read sma value
  sma <- SMA(Dax["Open"], sma_per)  #create sma vector
  Dax <- cbind(Dax, sma)            #add sma vector as new col
  
  # add prev day O-C
  Dax$OC <- Dax$Close - Dax$Open
  Dax$OC_P <- c(NA, Dax$OC[-length(Dax$OC)])
  
  # choose trades
  # 1. FTSE: a. Above SMA b. Prev Day UP => Go Short
  #Dax$Short <- ifelse(Dax$Open > Dax$sma, ifelse(Dax$OC_P >0, Dax$Open - Dax$Close, NA), NA)  
  #Dax$Short <- ifelse(Dax$Open > Dax$sma, Dax$Open - Dax$Close, NA)  #if O < SMA add to Long col
  
  # 2. Nik a. below SMA b. Prev UP => Go Short
  Dax$Short <- ifelse(Dax$Open < Dax$sma, ifelse(Dax$OC_P >0, Dax$Open - Dax$Close, NA), NA)
  
  # 2. Dow a. below SMA b. Prev Day Dn = Go Long
  #Dax$Long <- ifelse(Dax$Open < Dax$sma, Dax$Close - Dax$Open, NA) #if O > SMA add to Long col
  #Dax$Long <- ifelse(Dax$OC_P <0,Dax$Close - Dax$Open,NA)
  #Dax$Long <- ifelse(Dax$Open < Dax$sma, ifelse(Dax$OC_P <0,Dax$Close - Dax$Open,NA), NA)
  
  #print Long res
#   print(c(mkt, " - Long PL SMA", sma_per, round(sum(Dax$Long, na.rm=TRUE)))) 
#   print(sum(!is.na(Dax$Long[Dax$Long > 0])))
#   print(mean(Dax$Long[Dax$Long > 0],na.rm=TRUE))
#   print(sum(!is.na(Dax$Long[Dax$Long < 0])))
#   print(mean(Dax$Long[Dax$Long < 0],na.rm=TRUE))
#   print(sum(!is.na(Dax$Long)))
  
  
  #print short
  print(c(mkt, " - Short PL SMA", sma_per, round(sum(Dax$Short, na.rm=TRUE)))) #print Short res
  #round(sum(!is.na(Dax$Short[Dax$Short > 0])) / sum(!is.na(Dax$Short)) * 100)
  print(sum(!is.na(Dax$Short[Dax$Short > 0])))
  print(mean(Dax$Short[Dax$Short > 0],na.rm=TRUE))
  print(sum(!is.na(Dax$Short[Dax$Short < 0])))
  print(mean(Dax$Short[Dax$Short < 0],na.rm=TRUE))
  print(sum(!is.na(Dax$Short)))
}

#SMA_PrevD("Data//CommonDate//F100_2000_d.csv", 5, "FTSE")
SMA_PrevD("Data//CommonDate//Dow_2000_d.csv", 5, "Dow")
SMA_PrevD("Data//CommonDate//N225_2000_d.csv", 5, "Dow")
