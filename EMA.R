#  long-only exponential moving average crossover trading system
library(tidyverse)
library(lubridate)
library(tidyquant)
library(Rcpp)

# C code for tricky EMA calculation with no leading NA
sourceCpp(
  code = 
    "
     #include <Rcpp.h>
     // [[Rcpp::export]]
     Rcpp::NumericVector ewmaRcpp(Rcpp::NumericVector x, double a){
       int n = x.length();
       Rcpp::NumericVector s(n);
       s[0] = x[0];
       if (n > 1) {
         for (int i = 1; i < n; i++) {
           s[i] =  s[i-1] + (x[i] - s[i-1])/(( a + 1)/2);
         }
       }
       return s;
     }
    ")

R_path <- "Dropbox/Apps/R/projects" ; path <- getwd()
# if (!grep(R_path, path, ignore.case = TRUE))  setwd(R_path)

SPdata <- read_csv("SP----C.csv", col_names = FALSE)
# https://www.seykota.com/tribe/TSP/resources/SP----C.csv
colnames(SPdata) <- c("date", "open", "high", "low", "close", "volume", "open_int")
SPdata$date <- as_date(as.character(SPdata$date), format = "%Y%m%d")

SPdata |>
  ggplot(aes(x = date, y = close)) +
  geom_point(size = 1, shape = 4, alpha = 0.2) +
  geom_smooth(method = "lm")

# calculate ATR, average true range, for risk sizing the trades
SPdata <- SPdata |>
  select(date:close) |>
  mutate(hc_yest = high - lag(close),
         lc_yest = lag(close) - low,
         range = high - low) |>
  mutate(ATR = pmax(range, hc_yest, lc_yest, na.rm = TRUE)) 

# calculate EMA, exponential moving average, for low pass filter trade signals
slow_lag <- 150 # 150/15 generates $2,303,931.25 
fast_lag <- 15  # 325/85 yields $12,551,818.75
SPdata$atr_EMA <- ewmaRcpp(SPdata$ATR, 20) 
SPdata$slow <- ewmaRcpp(SPdata$close, slow_lag)
SPdata$fast <- ewmaRcpp(SPdata$close, fast_lag)

# EMA crossover
SPdata <- SPdata |>
  mutate(cross = fast - slow)  

# drop first 25 rows to let EMA `warm up`
SPdata <-  slice(SPdata, 26:n())  

# create trades from signals
SPdata <- SPdata |>
  mutate(on = ifelse(cross > 0 & lag(cross) < 0, 1, 0), 
         off = ifelse(cross < 0 & lag(cross) > 0, -1, 0),
         signal = on + off)

# buy trade details
SPdata <- SPdata |>
  mutate( buy_date = ifelse(on == 1, as_date(lead(date)), 0),
          buy_price = ifelse(on == 1, 
                             (lead(open) + lead(high))/2, 0))

# sell trade details
SPdata <- SPdata |>
  mutate(sell_date = ifelse(off == -1, as_date(lead(date)), 0),
         sell_price = ifelse(off == -1, 
                             (lead(open) + lead(low))/2, 0))

# Close out last trade if long when time (actually, data file) runs out
if (SPdata$cross[nrow(SPdata)] > 0) {
  SPdata$off[nrow(SPdata)] <- -1
  SPdata$sell_date[nrow(SPdata)] <- SPdata$date[nrow(SPdata)]
  SPdata$sell_price[nrow(SPdata)] <- SPdata$close[nrow(SPdata)] 
}

# Trade tables
buys <- SPdata |>
  select(atr_EMA:buy_price) |>
  filter(on == 1) |>
  select(starts_with("buy"), atr_EMA)

sells <- SPdata |>
  select(on:sell_price) |>
  filter(off == -1) |>
  select(starts_with("sell"))

trades <- bind_cols(sells, buys)
trades$buy_date <- as.Date(as.numeric(trades$buy_date))
trades$sell_date <- as.Date(as.numeric(trades$sell_date))

# calculate trade size based on pnl and book equity
trades <- trades |>
  mutate( closed_pnl = 1e6,    # $1,000,000 starting value
          buy_amt = 0,
          pnl = 0)

risk_budget <- 0.1  # aka Heat, is 10% of the book equity for each trade 
ATR_multiplier <- 5 # dynamic risk sizing based on current volatility via ATR

# calculate first row
trades$buy_amt[[1]] <- plyr::round_any((trades$closed_pnl[[1]] * 
                                          risk_budget) / (ATR_multiplier * trades$atr_EMA[[1]]), 250, f = round)
trades$pnl[[1]] = (trades$sell_price[[1]] - trades$buy_price[[1]]) * 
  trades$buy_amt[[1]]

# calculate the rest of the trade table
for (i in 2:nrow(trades)){
  trades$closed_pnl[[i]] <- trades$closed_pnl[[i-1]] + trades$pnl[[i-1]]
  
  trades$buy_amt[[i]] <- plyr::round_any((trades$closed_pnl[[i]] * 
                                            risk_budget) / (ATR_multiplier * trades$atr_EMA[[i]]), 250, f = round)
  
  trades$pnl[[i]] <- (trades$sell_price[[i]] - trades$buy_price[[i]]) * 
    trades$buy_amt[[i]]
}

print(sum(trades$pnl))
