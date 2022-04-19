# https://dplyr.tidyverse.org/articles/dplyr.html - the basic
# https://dplyr.tidyverse.org/articles/grouping.html - group by in details
# https://dplyr.tidyverse.org/articles/base.html - comparing dplyr functions
# to the base R function.

rm(list=ls())

library(tidyverse)
library(tidyquant)
library(lubridate)

#####Problem 1#####
# 1.Download the stock prices for AMZN, FB, NFLX, stocks from 2019-01-01 
# to 2021-04-01. Keep only the symbol/date/adjusted columns.

AMZN_prices <- tq_get("AMZN", complete_cases = TRUE, get = "stock.prices", from = "2019-01-01", to = "2021-04-01")
AMZN_prices <- AMZN_prices %>% select(symbol,date,adjusted)

FB_prices <- tq_get("FB", get = "stock.prices", from = "2019-01-01", to = "2021-04-01")
FB_prices <- FB_prices %>% select(symbol,date,adjusted)

NFLX_prices <- tq_get("NFLX", get = "stock.prices", from = "2019-01-01", to = "2021-04-01")
NFLX_prices <- NFLX_prices %>% select(symbol,date,adjusted)

# 2.Add all the missing dates(such as 2019-01-01), so that we have 
# observations for every single date. Fill in the missing values for adjusted 
# with the last non-missing observation.

#
AMZN_prices <- AMZN_prices %>% 
  complete(date = seq.Date(as.Date("2019-01-01"), max(date), by="day")) %>% 
  fill(symbol, .direction = "downup") %>%
  fill(adjusted, .direction = "downup")
        
FB_prices <- FB_prices %>% 
  complete(date = seq.Date(as.Date("2019-01-01"), max(date), by="day")) %>% 
  fill(symbol, .direction = "downup") %>%
  fill(adjusted, .direction = "downup")

NFLX_prices <- NFLX_prices %>% 
  complete(date = seq.Date(as.Date("2019-01-01"), max(date), by="day")) %>% 
  fill(symbol, .direction = "downup") %>%
  fill(adjusted, .direction = "downup")



# 3.Create a new data frame, which consist only of stocks from AMZN or FB and 
# has observations from 2019-01-01 to 2019-07-01 or 2020-04-01 to 2020-07-01. 
# Arrange the data frame first by the symbol name and by the date in 
# descending order.
common_prices <- full_join(AMZN_prices, FB_prices)
common_prices <- common_prices %>%  filter(date >= as.Date("2019-01-01"), date <= as.Date("2019-07-01"))
common_prices <- common_prices %>% arrange(symbol,desc(date))


# 4.Select the first and last observation of the aforementioned dataframe
# for each of the two stocks - AMZN and FB.

common_prices %>% group_by(symbol) %>% filter(row_number()==1 | row_number()==n())


# 5.Select the last observation for each stock, for each month. 
# In order to do this, first create a new column, which will show you the 
# year and the month. You can do this using the functions substr() or floor_date.

common_prices_month <- common_prices %>% mutate(month = month(date))

common_prices_month %>% group_by(symbol, month) %>% filter(row_number()==1)

#####Problem 1#####

#####Problem 2#####
#Use the dataframe from problem 1.2.
# Use the SMA function from the tidyquant package to calculate the 10day SMA 
# and the 26 day SMA for each of the 3 stocks. 

AMZ_sma <- AMZN_prices %>% 
  tq_mutate(select = adjusted, mutate_fun = SMA, n = 10, col_rename = "SMA10") %>% 
  tq_mutate(select = adjusted, mutate_fun = SMA, n = 26, col_rename = "SMA26")

FB_sma <- AMZN_prices %>% tq_mutate(select = adjusted, mutate_fun = SMA, n = 10, col_rename = "SMA10") %>% 
  tq_mutate(select = adjusted, mutate_fun = SMA, n = 26, col_rename = "SMA26")

NFLX_sma <- AMZN_prices %>% tq_mutate(select = adjusted, mutate_fun = SMA, n = 10, col_rename = "SMA10") %>% 
  tq_mutate(select = adjusted, mutate_fun = SMA, n = 26, col_rename = "SMA26")


# How many times did the 10 day SMA line cross 26 day SMA line from below? 
# How many times did the 10 day SMA line cross 26 day SMA line from above?
# You can take a look at this article: https://www.investopedia.com/trading/macd/
# Essentially by cross from above/below I want you to find the buy/sell signals.

AMZ_sma %>%
  select(date, adjusted, SMA10, SMA26) %>%
  pivot_longer(adjusted:SMA26, names_to = "Variable", values_to = "Price") %>%
  ggplot(aes(date, Price, group = Variable, color = Variable)) +
  geom_line(size = 1) +
  theme_bw() +
  labs(title = "AMZ with SMA10 and SMA26", x = "Date", y = "Price")

FB_sma %>%
  select(date, adjusted, SMA10, SMA26) %>%
  pivot_longer(adjusted:SMA26, names_to = "Variable", values_to = "Price") %>%
  ggplot(aes(date, Price, group = Variable, color = Variable)) +
  geom_line(size = 1) +
  theme_bw() +
  labs(title = "FB with SMA10 and SMA26", x = "Date", y = "Price")

NFLX_sma %>%
  select(date, adjusted, SMA10, SMA26) %>%
  pivot_longer(adjusted:SMA26, names_to = "Variable", values_to = "Price") %>%
  ggplot(aes(date, Price, group = Variable, color = Variable)) +
  geom_line(size = 1) +
  theme_bw() +
  labs(title = "NFLX with SMA10 and SMA26", x = "Date", y = "Price")


#We buy when the SMA10 crosses the SMA26 form below and the opposite, sell when the 
#SMA10 crosses the SMA from above

#example with Amazon, same with the others

AMZ_sma_strategy <- AMZ_sma %>%
  mutate(signal = ifelse(SMA26 > SMA10, "Buy","Sell"),
         previous_Signal = lag(signal),
         decision = case_when(signal == previous_Signal ~ "Hold", TRUE ~ signal)
         ) %>%
  filter(decision != "Hold") %>%
  select(date, adjusted, SMA10, SMA26, decision)
AMZ_sma_strategy






#####Problem 2#####
