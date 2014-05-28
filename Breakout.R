
myvec <- 1:83
myvec <- c(1:83)

alpha <- 1:10
length(alpha)
alpha[-length(alpha)]

beta <- c(NA,alpha[-length(alpha)])

gam = alpha + beta
gam.dset <- data.frame(alpha,beta)

length(alpha) <- 3

c("x","y")[rep(c(1,1,1,1), times=1)]
c("x","y")
rep(c(1,2,2,1),times=1)

chf <- read.csv("Swiss_90.csv",header=TRUE)
head(chf)
chf&newcol <- apply(chf,2,"AS")
head(chf)
str(chf)

a.length <- length(chf$Date)
chf&newcol <- apply(chf,2,length(chf$Date))
chf&newcol <- c(1:83)
chf["newcol"] <- NA
chf["newcol"] <- myvec
chf&newcol <- myvec

# You can add a column to your data using various techniques.
# 1. add vector
my.dataframe$new.col <- a.vector #or a calc from other cols
my.dataframe[, "new.col"] <- a.vector
my.dataframe["new.col"] <- a.vector

# 2. apply method

# 3. Transform() function
dataFrame <- transform(dataFrame, newColumnName = some equation)
dataFrame <- transform(dataFrame, newColumn = oldColumn1 + oldColumn2)

# 4. mapply() function


# Getting summaries
data(mtcars)
names(mtcars) # col names
# 1 col
mtcars$mpg
# 2 col
mtcars[, 2:4] # [] is rows,cols -> don't need first ,
mtcars[2:4]
# non-contiguos cols
mtcars[c(2,4)] #need c()
# - means exclude
mtcars[-c(2,4)]
# data by characteristic
mtcars$mpg > 20 # -> NOW what you want ...
mtcars[mtcars$mpg > 20, ] #want rows so see comma, then blank
mtcars[mtcars$mpg > 20, c(2,4)] #just certain cols ...
mtcars[mtcars$mpg > 20, c("mpg","hp")]
# attach
attach(mtcars)
mpg20 <- mpg > 20
detach()

str(mtcars)

# --------------------------------------------
chf[, D <- Close + c(NA, head(Open, -1))][]
chf&newcol
head(chf)

# prev row

dat$X3 <- c(0L, dat$ID[-1] <= dat$ID[-nrow(dat)])

previous.first.value <- c( "NA", chf[ - length(chf) ] )
head(previous.first.value)

DT <- data.table(A=1:5, B=1:5*10, C=1:5*100)
