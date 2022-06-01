# https://dplyr.tidyverse.org/articles/dplyr.html - the basic
# https://dplyr.tidyverse.org/articles/grouping.html - group by in details
# https://dplyr.tidyverse.org/articles/base.html - comparing dplyr functions
# to the base R function.

library(tidyverse)
library(tidyquant)


#####Problem 1#####
# Download data for a stock of your choice and do the following:
AMZN_prices <- tq_get("AMZN", complete_cases = TRUE, get = "stock.prices", from = "2022-01-10", to = "2022-04-28")
AMZN_prices <- AMZN_prices %>% select(symbol,date,adjusted)
AMZN_prices <- AMZN_prices %>% 
  complete(date = seq.Date(as.Date("2022-01-10"), max(date), by="day")) %>% 
  fill(symbol, .direction = "downup") %>%
  fill(adjusted, .direction = "downup")

AMZN_prices <-  AMZN_prices %>% tq_mutate(adjusted, periodReturn, period = "daily", col_rename = "returns")

# Calculate the 20 day SMA of the stock price and define upper and
# lower bounds around it which are equal to SMA +-2 standard deviation
# the past observations used to calculate the SMA.
std <- as.integer(summarise(AMZN_prices, sd = sd(adjusted)))

AMZN_prices <- AMZN_prices %>% 
  tq_mutate(select=adjusted, mutate_fun = SMA, n = 20) %>% 
  mutate(lowerbound = SMA -std, upperbound = SMA + std)



# Employ the following strategy and compare to a baseline strategy of buy and hold:
# If the price goes above the upper bound - sell.
# If the price goes below the lower bound - buy.
AMZN_prices <- AMZN_prices %>%
  mutate(strategie = ifelse( adjusted<lowerbound | adjusted<upperbound , "Buy","Sell"))

newdata <- na.omit(AMZN_prices)

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
print("With the new strategie, your have at the end: ")
print(portfolio)



#####Problem 1#####

#####Problem 2#####
# Calculate the RSI using the instruction about the formula from here:
# https://www.investopedia.com/terms/r/rsi.asp
# Employ the following strategy and compare to a baseline strategy of buy and hold:
# If the RSI above 65 - sell.
# If the price goes below 35 - buy.
AMZN_prices <- tq_get("AMZN", complete_cases = TRUE, get = "stock.prices", from = "2022-01-10", to = "2022-04-28")
AMZN_prices <- AMZN_prices %>% select(symbol,date,adjusted)
AMZN_prices <- AMZN_prices %>% 
  complete(date = seq.Date(as.Date("2022-01-10"), max(date), by="day")) %>% 
  fill(symbol, .direction = "downup") %>%
  fill(adjusted, .direction = "downup")

AMZN_prices <- AMZN_prices %>% 
  tq_mutate(select=adjusted, mutate_fun = RSI)

AMZN_prices <- AMZN_prices %>%
  mutate(strategie = ifelse( rsi<35 | rsi<65 , "Buy","Sell"))

newdata <- na.omit(AMZN_prices)

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
print("With the RSI strategie, your have at the end: ")
print(portfolio)









#####Problem 2#####

#Upload your homeworks on your own github repo and send me an email, when you are done.
