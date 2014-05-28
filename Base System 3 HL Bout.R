
# Build Function
PrevHLBout <- function(file, SLoss, rnd=0){
  Mkt <- read.csv(file,header=TRUE) #read file
  Mkt$prevHigh <- c( NA, Mkt$High[ - length(Mkt$High) ] )
  Mkt$prevLow <- c( NA, Mkt$Low[ - length(Mkt$Low) ] )
  #print(tail(Mkt))
  #print(head(Mkt))
  # Long
  Mkt$Long <- ifelse(!is.na(Mkt$prevHigh), ifelse(Mkt$High > Mkt$prevHigh, Mkt$Close - Mkt$prevHigh, NA), NA)
  print(c("Long",sum(Mkt$Long, na.rm=TRUE)))
  # Short
  Mkt$Short <- ifelse(!is.na(Mkt$prevLow), ifelse(Mkt$Low < Mkt$prevLow, Mkt$prevLow - Mkt$Close, NA), NA)
  print(c("Short",sum(Mkt$Short, na.rm=TRUE)))
  
  #SLoss
  Mkt$Long <- ifelse(!is.na(Mkt$Long), ifelse(Mkt$Long < SLoss, SLoss, Mkt$Long), NA)
  lg_pl <- round(sum(Mkt$Long, na.rm=TRUE),rnd)
  print(c("Long S Loss",lg_pl, "Year", lg_pl/13))
  #print(c("Long S Loss",round(sum(Mkt$Long, na.rm=TRUE),rnd)))
}

# Call Function
PrevHLBout(file="Data//CommonDate//Dax_2000_d.csv", SLoss=-40)
PrevHLBout(file="Data//CommonDate//CAC_2000_d.csv", SLoss=-40)
PrevHLBout(file="Data//CommonDate//F100_2000_d.csv", SLoss=-40)
PrevHLBout(file="Data//CommonDate//Dow_2000_d.csv", SLoss=-100)
PrevHLBout(file="Data//CommonDate//N225_2000_d.csv", SLoss=-100)
PrevHLBout(file="Data//CommonDate//Oz_2000_d.csv", SLoss=-40)

PrevHLBout(file="Data//CommonDate//GBUS_d.csv", SLoss=-4000, rnd=4)
