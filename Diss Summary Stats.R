# Data Summary Stats


# ------------------------------------------------------------------------------------
source("Diss Summary Stats SOURCE.R") #pull in the functions

Mkt <- read.csv("Data//CommonDate//Dax_2000_d.csv",header=TRUE)
Mkt <- read.csv("Data//CommonDate//F100_2000_d.csv",header=TRUE)
Mkt <- read.csv("Data//CommonDate//CAC_2000_d.csv",header=TRUE)

Mkt <- read.csv("Data//CommonDate//Dow_2000_d.csv",header=TRUE)
Mkt <- read.csv("Data//CommonDate//Dow_96_tick_d.csv",header=TRUE)

Mkt <- read.csv("Data//CommonDate//N225_2000_d.csv",header=TRUE)
Mkt <- read.csv("Data//CommonDate//Oz_2000_d.csv",header=TRUE)

Mkt$prevHigh <- c( NA, Mkt$High[ - length(Mkt$High) ] )
Mkt$prevLow <- c( NA, Mkt$Low[ - length(Mkt$Low) ] )
Mkt$OH <- Mkt$High - Mkt$Open
Mkt$OL <- Mkt$Open - Mkt$Low
#Mkt$absOC <- abs(Mkt$Open - Mkt$Close)

cars <- c(1, 3, 6, 4, 9)
plot(cars)

# --- Open
OpenBetPrevHL(Mkt)
OpenBetPrevHL1090(Mkt[complete.cases(Mkt),])
OpenBetPrevHL3070(Mkt[complete.cases(Mkt),])
# # --- Close
CloseOutPrevHL(Mkt[complete.cases(Mkt),])
CloseOutPrevHL1090(Mkt[complete.cases(Mkt),])
CloseOutPrevHL3070(Mkt[complete.cases(Mkt),])
CloseBetTodayHL1090(Mkt[complete.cases(Mkt),])
CloseBetTodayHL3070(Mkt[complete.cases(Mkt),])
# ----- CurrHL
CurrHLCrossPrevHL(Mkt[complete.cases(Mkt),])
CurrHLCrossBothPrevHL(Mkt[complete.cases(Mkt),])
CurrHLCrossPrevHL1090(Mkt[complete.cases(Mkt),])
CurrHLCrossBothPrevHL1090(Mkt[complete.cases(Mkt),])
