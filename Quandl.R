
Quandl - auth: mW11caB1btTqNnBWGhtg

install.packages("Quandl")
library(Quandl)

# Sometimes a more recent version of the Quandl package may be available on Github. 
# (CRAN packages often update with a lag). 
# To download and install the latest version from Github, enter this code snippet:
# install.packages("devtools")
# library(devtools)
# install_github('R-package','quandl')
# library(Quandl)

# enter auth code
Quandl.auth("mW11caB1btTqNnBWGhtg")

# The Quandl package is able to return data in 4 very usable formats: 
# data frame ("raw"), 
# ts ("ts"), 
# zoo ("zoo") and 
#xts ("xts"). The default is "raw". 

mydata = Quandl("FRED/GDP")
mytimeseries = Quandl("NSE/OIL", type="ts")
mydata = Quandl("NSE/OIL", start_date="yyyy-mm-dd", end_date="yyyy-mm-dd")


# ---------- Get New Data Fnc --------------------
Update_from_yahoo <- function(x,st,en, Mkt){
  start <- format(Sys.Date(), format="%Y-%m-%d")
  end <- format((Sys.Date() - x), format="%Y-%m-%d")
  ydax = Quandl("YAHOO/INDEX_GDAXI", start_date=st, end_date=en)
  ydax <- ydax[,c(1,2,3,4,5)]
  ydax <- ydax[order(ydax$Date),]
  ydax$Date <- as.character(ydax$Date)

  Mkt <- rbind(Mkt,ydax)
  Mkt <- unique(Mkt)
  return(Mkt)
}

dax_csv <- read.csv(("../Data/Dax_2000.csv"),stringsAsFactors = FALSE)
dd <- Update_from_yahoo(40,"2014-03-30","2014-05-02", dax_csv)
tail(dd)
nrow(dd)
nrow(unique(dd))


start <- format(Sys.Date(), format="%Y-%m-%d")
end <- format((Sys.Date() - 20), format="%Y-%m-%d")
ydax = Quandl("YAHOO/INDEX_GDAX", start_date=start, end_date=end)

# Yahoo
#YAHOO/INDEX_GDAXI - "2014-04-01" format
ydax = Quandl("YAHOO/INDEX_FCHI", start_date="2000-01-01", end_date="2014-05-10")
ydax = Quandl("YAHOO/INDEX_DJI", start_date="2000-01-01", end_date="2014-10-10")
ydax <- ydax[,c(1,2,3,4,5)]
ydax <- ydax[order(ydax$Date),]
ydax$Date <- as.character(ydax$Date)
tail(ydax)
head(ydax)
write.csv(ydax,paste('../Data/Dow2_2000.csv',sep=""),row.names=FALSE)

+str(ydax)
str(dax_csv)

dax_csv <- read.csv(("../Data/Dax_2000.csv"),stringsAsFactors = FALSE)
#dax_csv$Date <- as.POSIXct(dax_csv$Date)

tail(dax_csv$Date)
dd2 <- unique(dd)
tail(dd2,n=30)
nrow(dd)
nrow(dd2)

dd <- rbind(dax_csv,ydax)
tail(dd,n=30)

dd$Date <- as.character(dd$Date)

dd2 <- unique(dd)
tail(dd2$Date,n=30)
write.csv(ydax,"../Data/Dax_2000_test.csv", row.names=FALSE)




# Google
GOOG/SWX_LYDAX_EUR - "2014-04-01" format
gdax = Quandl("GOOG/SWX_LYDAX_EUR", start_date="2014-04-01", end_date="2014-05-01")
tail(gdax)

setwd("D:/Allan/DropBox/RWorkingDir/Trading/Dax")
# - 24/03/2014 - format
dax_csv <- read.csv(("../Data/Dax_2000.csv"))
dax_csv$Date <- as.POSIXct(dax_csv$Date,format='%d/%m/%Y')
tail(dax_csv)
tail(format(dax_csv$Date, format="%d/%m/%Y"))

dax_csv$Date2 <- format(dax_csv$Date, format="%Y-%m-%d")
tail(dax_csv)

# -----------------------------
Sys.Date()
Sys.Date() - 5
today <- Sys.Date()
today - 5
format(today, format="%Y-%m-%d")
format(today, format="%b %A %d %Y")

Symbol  Meaning	                Example
%d	    day as a number (0-31)	01-31
%a      abbreviated weekday     Mon
%A	    unabbreviated weekday	  Monday

%m	    month (00-12)	          00-12
%b      abbreviated month       Jan
%B	    unabbreviated month     January

%y   2-digit year   07
%Y	 4-digit year 2007

# as.Date( ) function to convert character data to dates
# You can convert dates to character data using the as.Character( ) function

# ---------------------------------------------

fil <- c("../../Data//Dax_2000_d.csv",
         "../../Data//CAC_2000_d.csv", 
         "../../Data//F100_2000_d.csv",
         "../../Data//Dow_2000_d.csv",
         "../../Data//N225_2000_d.csv",
         "../../Data//Oz_2000.csv")

#nm <- c("Dax", "CAC", "F100", "Dow", "Nik", "Oz")
#df10 <- as.data.frame(matrix(seq(11),nrow=1,ncol=11)) # to hold results

addTA <- function(fil){
  browser()
  for(i in 1:length(fil)){
    Mkt <- read.csv(fil[i])
    #aroon
    ar <- aroon(Mkt$Close, n=20)
    Mkt <- cbind(Mkt, ar)
    
  }
}

addTA(fil)

#stoch
st <- stoch(Dax$Close) #HL
Dax <- cbind(Dax,st)
#roc
Dax$mom <- momentum(Dax$Close,n=12) #HL
lw <- quantile(Dax$mom, na.rm=T, probs=0.25) 
hi <- quantile(Dax$mom, na.rm=T, probs=0.75)
Dax$lw <- lw
Dax$hi <- hi
#candlestick
Dax$Date <- as.POSIXct(Dax$Date,format='%d/%m/%Y')
Dax_xts <- xts(Dax[,c(2,3,4,5)],Dax$Date)
hh <- as.data.frame(CSPHammer(Dax_xts))
hi <- as.data.frame(CSPInvertedHammer(Dax_xts))
Dax <- cbind(Dax,hh)
Dax <- cbind(Dax,hi)
Dax$Date <- as.character.Date(index(Dax_xts),format="%d %b")
Dax <- cbind(Dax,Date)

# ----------------------------
setwd("D:/Allan/DropBox/RWorkingDir/Trading/Dax")
Dax <- read.csv("../Data/Dax_2000.csv",stringsAsFactors = FALSE)
Dax$OH <- Dax$High - Dax$Open
Dax$OL <- Dax$Open - Dax$Low
Dax$mn <- ifelse(Dax$OH>Dax$OL,Dax$OL,Dax$OH)
quantile(Dax$mn, probs=0.90)
nr<- nrow(Dax)
dd <- Dax$mn[3554:3654]
quantile(Dax$mn[3554:3654], probs=0.90)
quantile(Dax$mn[(nr-100):nr], probs=0.90)
nr-100
