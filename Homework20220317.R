#####Problem 1#####
#Write a loop which simulates 1000 times a martingale strategy based on a coin flip
#Martingale is a gambling strategy where you multiply your next bet twice if
#you have lost your previous one. You bet 1 and you win. Because you won you bet 1
# again. You lose. Then you bet 2, you lose again. You bet four and you win.
#Because you won, you go back to betting one etc. You start with 100 USD and you
#base bet is one. 
#If the coin flip is biased and you have 48.60% chance to win, when do you
#go broke on average(out of those 1000 simulations)? Look at the help for sample,
#to figure out how to pick incorporate the 48.6% probability.
#You can use a while loop for simulating when you go broke. A while loop
#loops until a condition is TRUE. Example:
# i <- 1
# while (i < 6) {
#   print(i)
#   i <- i + 1
# } 
#In your case you want to loop until your budget is > 0.
# Budget <- 100
# while (Budget > 0) {
#   Do something
# } 
#Pay attention to the fact that you can't bet more money than you have.
#If you lose 1, 2, 4, 8, 16, 32. Then your remaining money will be 
#100-32-16-8-4-2-1 = 37, so you can bet max 37 USD.
#####Problem 1#####

Budget <- 100
bet = 1
for(k in 1:1000){
  while (Budget > 0) {
    Budget <- Budget - bet
    if(sample(c(0,1), 1)==0){ 
      bet <- 1
    }
    else{
      Budget <- Budget + 2*bet
    }
  } 
}
print(Budget)

ResultsVector <- NULL
for(k in 1:1000){
  Budget <- 100
  bet = 1
  count <- 0
  while (Budget > 0) {
    count <- count + 1
    Budget <- Budget - bet
    if(sample(c(0,1),1,prob=c(1-0.486,0.486))==0){ 
      bet <- 1
    }
    else{
      Budget <- Budget + 2*bet
    }
  } 
  ResultsVector <- c(ResultsVector, count)
}

sum(ResultsVector)/1000


#####Problem 2#####
# Read everything from https://r4ds.had.co.nz/transform.html, up until
# 5.6 Grouped summaries with summarise(). If you want to, you can
# read everything and then https://r4ds.had.co.nz/relational-data.html

#Do all the exercises:
# 5.2.4 Exercises----
#1
filter(flights, arr_delay >= 2)
filter(flights, dest == 'IAH' |dest == 'HOU')
filter(flights, carrier == 'UA' |dest == 'DELTA')
filter(flights, month == 7|month == 8 | month == 9)
filter(flights, arr_delay >= 2 & dep_delay == 0)
filter(flights, dep_delay > 1 & dep_delay - 0.5 < arr_delay)
filter(flights, dep_time >= 0 & dep_time <= 600)

#2
filter(flights, between(dep_time, 0, 600))

#3
filter(flights, is.na(dep_time))
#dep_delay arr_time and arr_delay
#probably flights that have been cancelled

#4
#by convention in maths all value power of 0 is equal to one
#
# FALSE and any value is equal to False by logic
# is NA because of the rules of limit Inf or 1/Inf for instance


# 5.3.1 Exercises----
#1
arrange(flights, desc(is.na(dep_delay)))
#2
arrange(flights, desc(dep_delay))
#3
arrange(flights, air_time)
#4
arrange(flights, desc(distance)) #flight from JFK to HNL 
arrange(flights, distance) #flight from EWR to LGA 


# 5.4.1 Exercises----
#1
select(flights, dep_time, dep_delay, arr_time, arr_delay)
select(flights, dep_time:arr_delay)
select(flights, -(year:day), -(carrier:time_hour))
select(flights, starts_with("dep"), starts_with("arr"))
select(flights, contains("dep"), contains("arr"))

#2
#it selects only one time the columns

#3
#any of matches variable names in a character vector
#with the vector vars we can select columns with the name in this vector
vars <- c("year", "month", "day", "dep_delay", "arr_delay")
select(flights, any_of(vars))

#4
#yes because time in columns are in minuscule but we demand for tim in majuscule
#select help us and normalizes text in minuscule

  
# 5.5.2 Exercises 
#1
mutate(flights,
          dep_time = dep_time /100,
          sched_dep_time = sched_dep_time/100
)
#2
transmute(flights,
          air_time,
          arr_time - dep_time
          )
#values are different, we can modifiy the value of air time with the difference
mutate(flights,
       air_time = arr_time - dep_time
)

#3
transmute(flights,
          dep_time, 
          sched_dep_time, 
          dep_delay
)
#dep_delay = dep_time - sched_dep_time

#4
ranking <- mutate(flights,
                  rank_delay = min_rank(desc(dep_delay)) 
                  )
arrange(ranking, rank_delay)


select

#5
#an error because we can addition objects of different length

#6
#cos(x), sin(x),tan(x),acos(x),asin(x),atan(x),atan2(y, x),cospi(x),sinpi(x),tanpi(x)

#You can also read the official dplyr site.
#https://dplyr.tidyverse.org/index.html
#https://dplyr.tidyverse.org/articles/dplyr.html
#####Problem 2#####

#Copy to the recording https://unisofiafaculty.sharepoint.com/:v:/r/sites/AccountingFinanceandDigitalapplicationsSeminargroupI/Shared%20Documents/General/Recordings/Quantitative%20methods%20in%20finance%202022-20220317_180538-Meeting%20Recording.mp4?csf=1&web=1&e=3m90AT
