# https://dplyr.tidyverse.org/articles/dplyr.html - the basic
# https://dplyr.tidyverse.org/articles/grouping.html - group by in details
# https://dplyr.tidyverse.org/articles/base.html - comparing dplyr functions
# to the base R function.

rm(list=ls())

library(tidyverse)
library(tidyquant)


# Update your quantmod library, or you might not be able to download the data.
# version should be 0.4.2.
# Here is a link to discussion of the problem with getting data from yahoo.
# https://github.com/joshuaulrich/quantmod/issues/358
#####Problem 1#####
# Write the following functions:

#Data init
AMZN_prices <- tq_get("AMZN", complete_cases = TRUE, get = "stock.prices", from = "2019-01-01", to = "2022-05-09")
AMZN_prices <- AMZN_prices %>% select(symbol,date,adjusted)
AMZN_prices <- AMZN_prices %>% 
  complete(date = seq.Date(as.Date("2019-01-01"), max(date), by="day")) %>% 
  fill(symbol, .direction = "downup") %>%
  fill(adjusted, .direction = "downup")

FB_prices <- tq_get("FB", complete_cases = TRUE, get = "stock.prices", from = "2019-01-01", to = "2022-05-09")
FB_prices <- FB_prices %>% select(symbol,date,adjusted)
FB_prices <- FB_prices %>% 
  complete(date = seq.Date(as.Date("2019-01-01"), max(date), by="day")) %>% 
  fill(symbol, .direction = "downup") %>%
  fill(adjusted, .direction = "downup")

# 1.1. A function which replicates the SMA function. It should calculate the SMA
# for a vector to the left:
replicateSMA <- function(data,n){
  pricesVector <- as.vector(pull(data,adjusted))
  lg <- length(pricesVector)
  sma <- replicate(n-1,0)
  for (i in n:lg){
    value <- mean(pricesVector[c(i-n+1,i)])
    sma <- c(sma,value)
  }
  
  return (sma)
}
replicateSMA(FB_prices,3)

# 1.2. A function, which calculates the correlation coefficient between two vectors.
# It should replicate the cor function from the base package.

#I've adapted the code for the prices, we suppose they have the same length
replicateCor <- function(data1,data2){
  Vector1 <- as.vector(pull(data1,adjusted))
  Vector2 <- as.vector(pull(data2,adjusted))
  meanVector1 <- mean(Vector1)
  meanVector2 <- mean(Vector2)
  lg <- length(Vector1)
  cov <- 0
  var1 <- 0
  var2 <- 0
  for (i in 1:lg){
    cov <- cov + (Vector1[i]-meanVector1)*(Vector2[i]-meanVector2)
    var1 <- var1 + (Vector1[i]-meanVector1)**2
    var2 <- var2 + (Vector2[i]-meanVector2)**2
  }
  return( cov / (var1*var2)**0.5)
  
}
replicateCor(AMZN_prices,FB_prices)

#####Problem 1#####

#####Problem 2#####
# Find all prime numbers less than 100, using for/while loops.
PrimeVector <- c(2)
for (i in 2:100){
  flag <- TRUE
  for (j in 2:sqrt(i)){
    if (i%%j==0){
      flag <- FALSE
      break
    }
  }
  if (flag==TRUE){
    PrimeVector <- c(PrimeVector,i)}
}
PrimeVector
#####Problem 2#####

#####Problem 3#####
# Read the wikipedia article and investopedia article on MACD:
# https://en.wikipedia.org/wiki/MACD
# https://www.investopedia.com/terms/m/macd.asp

# Download data for a stock of your choice and do the following:
# 1.Calculate the 26-period EMA(use the EMA function from tidyquant)
# 2.Calculate the 12-period EMA.

newdata <- AMZN_prices %>% mutate(EMA26 = EMA(adjusted, n = 26),EMA12 = EMA(adjusted, n = 12))

# 3.Calculate the MACD line(12-period EMA minus 26-period EMA)

newdata <- newdata %>% mutate(MACD = EMA12 - EMA26)

# 4.Calculate the signal line - this is the 9-period EMA of the MACD.

newdata <- newdata %>% mutate(signal = EMA(MACD,n=9))


# 5.Calculate the buy/sell signals. This means create a new column which tell
# us if we should buy or sell. When the MACD line crosses the signal line
# from above(MACD is above signal then MACD is below signal) this is a sell signal. 
# If it crosses from below (MACD is below signal then MACD is above signal) this is a buy signal.

newdata <- newdata %>%
  mutate(strategie = ifelse(signal > MACD, "Buy","Sell"))


#we delete the first row with NA in the decision

newdata <- na.omit(newdata)



# 6. Simulate how the strategy preforms and compare it to a benchmark strategy
# of just buying and holding the stock.
# In order to do this start with a portfolio of 100$ invested in the stock on the first day
# and see how it performs. Example:
# I start with 100$ and a stock which costs 100$ at the beginning of my time period.
# I get a buy signal when the stock price is 90. I buy the stock.
# I get a sell signal to sell the stock when the price is 110. I sell it and 
# and don't get any more signals.I end up with 100 * 110 / 90 = 122.22 
# The benchmark portfolio is I buy the stock at 100 at the beginning and at
# the end of the period the stock price is 120. I end up with 120.
# 122.22 > 120. so the MACD strategy was beating the market.


#we test the strategie for the period of 2019-01-01 and 2022-05-09
lg <- length(newdata$adjusted)
benchmark_return <- (newdata$adjusted[lg] - newdata$adjusted[1]) / newdata$adjusted[1]
benchmark_return

pt <- newdata$adjusted[1]
portfolio<-100
hold <- FALSE
for (t in 1:lg){
  if(newdata$strategie[t]=="Buy" && hold==FALSE){
    pt <- newdata$adjusted[t]
    hold <- TRUE
    print(portfolio)
  } else if (newdata$strategie[t]=="Sell" && hold==TRUE){
    ptt <- newdata$adjusted[t]
    price_return <- (ptt-pt)/pt
    portfolio <- portfolio * (1+price_return)
    hold <-FALSE
    print(portfolio)
  }
}
portfolio




print("Portfolio initial: 100$")
print("With the benchmark strategie, your have at the end: ")
print((1+benchmark_return)*100)
print("With the MACD strategie, your have at the end: ")
print(portfolio)

#So in this large period of time, it's better to not use this MACD strategie
#But in some period, it could be interesting 


#####Problem 3#####
#Upload your homeworks on your own github repo and send me an email, when you are done.