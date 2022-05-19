library(tidyverse) 
library(tidyquant) 
library(lubridate) 
library(quantmod) 

# https://dplyr.tidyverse.org/articles/dplyr.html - the basic
# https://dplyr.tidyverse.org/articles/grouping.html - group by in details
# https://dplyr.tidyverse.org/articles/base.html - comparing dplyr functions
# to the base R function.

# Update your quantmod library, or you might not be able to download the data.
# version should be 0.4.2.
# Here is a link to discussion of the problem with getting data from yahoo.
# https://github.com/joshuaulrich/quantmod/issues/358 

#####Problem 1#####
# Write the following functions:
# 1.1. A function which replicates the SMA function. It should calculate the SMA
# for a vector to the left: 
library(TTR)

price <- c(1, 8, 5, 9, 11, 14)
newSMA <- function (price,n){
  f <- c()
  for (i in n:length(price)){
    f[i]<-mean(price[(i-n+1):i])
  }
  return(f)
} 

newSMA(price, n=2) 

SMA(price, n=2)

# 1.2. A function, which calculates the correlation coefficient between two vectors.
# It should replicate the cor function from the base package. 
a <- c(1,2,3,5,6,8,9)
a <- InputVector
b <- c(2,4,6,9,12,15,18)
b <- InputVector1
correlation <- function(InputVector, InputVector1){
  Sumdiff = length(InputVector)*sum(InputVector*InputVector1)-sum(InputVector)*sum(InputVector1)
  FinalResult <- Sumdiff/sqrt((length(InputVector)*sum(InputVector^2) - sum(InputVector)^2)*(length(InputVector1)*sum(InputVector1^2)-sum(InputVector1)^2))
  return(FinalResult)
}

correlation(a,b) 

cor(a, b)
#####Problem 1#####

#####Problem 2#####
# Find all prime numbers less than 100, using for/while loops. 

for(i in 1:100){
  if(i==1){
    next
  }
  else if(i==2){
    i =2}
  else if(i%%2==0){
    next}
  else if(i==3){
    i=3}
  else if(i%%3==0){
    next
  }
  else if (i==5){
    i=5}
  else if(i%%5==0){
    next
  }
  else if(i==7){
    i=7}
  else if(i%%7==0){
    next
  }
  print(i)}





 




#####Problem 2#####

#####Problem 3#####
# Read the wikipedia article and investopedia article on MACD:
# https://en.wikipedia.org/wiki/MACD
# https://www.investopedia.com/terms/m/macd.asp

# Download data for a stock of your choice and do the following: 
stock_prices <- tidyquant::tq_get("NKE")
# 1.Calculate the 26-period EMA(use the EMA function from tidyquant) 
stock_prices <- tidyquant::tq_get("NKE") %>% 
dplyr::mutate(EMA26 = TTR::EMA(stock_prices$adjusted, n = 26 ))

# 2.Calculate the 12-period EMA.
stock_prices <- tidyquant::tq_get("NKE") %>% 
  dplyr::mutate(EMA26 = TTR::EMA(stock_prices$adjusted, n = 26 ),
                EMA12 = TTR::EMA(adjusted, n = 12))
# 3.Calculate the MACD line(12-period EMA minus 26-period EMA) 
stock_prices <- tidyquant::tq_get("NKE") %>% 
  dplyr::mutate(EMA26 = TTR::EMA(stock_prices$adjusted, n = 26 ),
                EMA12 = TTR::EMA(adjusted, n = 12), 
                MACD_line = EMA12 - EMA26)
# 4.Calculate the signal line - this is the 9-period EMA of the MACD.
stock_prices <- tidyquant::tq_get("NKE") %>% 
  dplyr::mutate(EMA26 = TTR::EMA(stock_prices$adjusted, n = 26 ),
                EMA12 = TTR::EMA(adjusted, n = 12), 
                MACD_line = EMA12 - EMA26, 
                Signal_Line = TTR::EMA(MACD_line, n =9))
# 5.Calculate the buy/sell signals. This means create a new column which tell
# us if we should buy or sell. When the MACD line crosses the signal line
# from above(MACD is above signal then MACD is below signal) this is a sell signal. 
# If it crosses from below (MACD is below signal then MACD is above signal) this is a buy signal. 

stock_prices <- tidyquant::tq_get("NKE") %>% 
  dplyr::mutate(EMA26 = TTR::EMA(adjusted, n = 26 ),
                EMA12 = TTR::EMA(adjusted, n = 12), 
                MACD_line = EMA12 - EMA26, 
                Signal_Line = TTR::EMA(MACD_line, n =9)) %>% 
  ungroup() %>%
  dplyr::filter(!is.na(MACD_line & EMA26)) %>% 
  dplyr::mutate(cross = case_when(MACD_line > Signal_Line & dplyr::lag(Signal_Line) > dplyr::lag(MACD_line) ~ "buy",
                                  MACD_line < Signal_Line & dplyr::lag(Signal_Line)< dplyr::lag(MACD_line)~ "sell",
                                  TRUE ~ "hold"))



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
Benchmark <- 100
Buy_hold <- stock_prices %>% 
  mutate(product_coeff = adjusted/lag(adjusted), 
         Price_Benchmark = Benchmark*product_coeff, 
         decision = case_when(Price_Benchmark > 100 ~ "hold",
                              Price_Benchmark < 100 ~ "buy",
                              TRUE ~ "indifferent"))

#####Problem 3#####
#Upload your homeworks on your own github repo and send me an email, when you a re done.

#the idea is to see when the lines cross(time and price), at a certain point they are crossing
#DOES THE STRATEGY BEAT THE MARKET,is it working, is it profitable 
#if you dont have a stock, use 1
#