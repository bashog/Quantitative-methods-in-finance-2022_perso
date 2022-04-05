#####Problem 1#####----
# Write a function, which uses a loop to calculate factorial.
# The base R function is called factorial and you should replicate its result.
# This is a function, which takes two numbers, multiplies them and returns
# the result as output:
# MultiplicationFunction <- function(inputNumber1, inputNumber2){
#   Result <- inputNumber1 * inputNumber2
#   return(Result)
# }
# MultiplicationFunction(5, 3)
# 
# Write a factorial function:
FactorialFunction <- function(n) {
  if(n==0) {
    return(1)
  } else {
    return(n * FactorialFunction(n-1))
  }
}
FactorialFunction(5)

# FactorialFunction <- function(inputNumber){
#   ???
#     return(Result)
# }
#####Problem 1#####

#####Problem 2#####----
#Write a function which takes a vector and returns its standard deviation.
#You should get the same results as the sd() function.
SDFunction <- function(inputVector){
  n <- length(inputVector)
  meanV <- mean(inputVector)
  diff <- inputVector - meanV
  varianceV <- sum( diff*diff ) / n
  return(sqrt(varianceV))
}

x <- c(1,2,3,4)
isNotResult <- sd(x)
isNotResult
isResult <- SDFunction(x) 
isResult
#####Problem 2#####

#####Problem 3##### ----
# Read everything from , 
# in particular chapters 5.6/5.7
library(nycflights13)
library(tidyverse)
nycflights13::flights

#Do all the exercises:
# 5.6.7 Exercises----

#1
#A flight is 15 minutes early 50% of the time, and 15 minutes late 50% of the time.
flight_delay_15 <- group_by(flights, flight) %>% summarise(num_flights = n(),
                                                                percentage_15_mins_early = sum(sched_arr_time - arr_time == 15)/num_flights,
                                                                percentage_15_mins_late = sum(arr_time - sched_arr_time == 15)/num_flights)
flight_delay_15 %>% filter(percentage_15_mins_early == 0.5 & percentage_15_mins_late == 0.5)


#2
not_cancelled <- flights %>% filter(!is.na(dep_delay), !is.na(arr_delay))

not_cancelled %>% group_by(dest) %>% summarise(n = n())

not_cancelled %>% group_by(tailnum) %>% summarise(n = sum(distance))

#3
#if a flight depart he will arrived (and the opposite), so one of the both is enough
cancelled <- flights %>% filter(is.na(arr_delay)) #or
cancelled <- flights %>% filter(is.na(arr_delay))

#4
view(flights)
cancelled_per_day <- flights %>% filter(is.na(arr_delay)) %>% group_by(year,month,day) %>% summarise(count=n())
cancelled_per_day <-mutate(cancelled_per_day, dep_date = lubridate::make_datetime(year, month, day), count)
ggplot(data = cancelled_per_day, mapping = aes(x=dep_date ,y = count))

#5
#OO, YV and 9E
flights %>% filter(arr_delay > 0) %>% group_by(carrier) %>% 
  summarise(av_arr_delay = mean(arr_delay, na.rm=TRUE)) %>%
  arrange(desc(av_arr_delay))
  

#6
#The sort argument to count() do a sort of the count to recognize the bigger group (which are often the most revelant)

# 5.7.1 Exercises
#####Problem 3#####


#####Problem 4#####
#Find the following:
#4.1 For each carrier what is the most common destination?
flights %>% 
  group_by(carrier, dest) %>% 
  count(dest) %>%
  group_by(carrier) %>%
  filter(rank(desc(n)) < 2)

#4.2 For each carrier what is the biggest delay?
flights %>% 
  group_by(carrier) %>% 
  summarise(carrier,delay = arr_delay - dep_delay) %>%
  group_by(carrier) %>%
  filter(rank(desc(delay)) < 2)


#4.3 Which are the three plane which have flown the most/least miles?
flights %>% arrange(distance) 
flights %>% arrange(desc(distance))


#4.4 What are the first/last flights for each day in February 2013?
filter(flights,year==2013,month==12) %>% arrange(dep_time) 
filter(flights,year==2013,month==12) %>% arrange(desc(dep_time))
  
  
#4.5 Which company flew the most miles in March 2013? Which flew the least?
filter(flights,month==3,year==2013) %>% arrange(desc(distance))
filter(flights,month==3,year==2013) %>% arrange(air_time)

#4.6 Which month had the most delays over 60 minutes?
flights %>% mutate(delay=arr_delay-dep_delay) %>% filter(delay > 60) %>% 
  group_by(month) %>% summarise(n=n()) %>%
  arrange(desc(n))


#4.7 What is the average time between two consecutive flights?

#4.8 Use the SDFunction function from exercise 2 to calculate the standard deviation
#of the flight delays for each month and for each destination.
#####Problem 4#####

#Upload your homeworks on your own github repo.
#Link to the seminar https://unisofiafaculty.sharepoint.com/:v:/s/AccountingFinanceandDigitalapplicationsSeminargroupI/EfR2uYarKcRFiljWMgRb9U8BL6XsygAzJv_fu7mOCQsYzQ?e=CanPG0
