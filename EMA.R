#  long-only exponential moving average crossover trading system
rm(list=ls())
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

# load the data file  # https://www.seykota.com/tribe/TSP/resources/SP----C.csv
SPdata <- read_csv("SP----C.csv",
     col_names = c("date", "open", "high", "low", "close", "volume", "open_int"),
     col_types = list(
       date = col_date(format = "%Y%m%d"),
       open = "d",
       high = "d",
       low = "d",
       close = "d",
       volume = "d",
       open_int = "d"
     )
     )

# pretty graph
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
SPdata$atr_EMA <- ewmaRcpp(SPdata$ATR, 20)

# start the optimization here, right here
steamroller <- data.frame(matrix(ncol = 20, nrow = 20))
steampnl <- data.frame(matrix(ncol = 20, nrow = 20))

SPdata_orig <- SPdata

for(slow in 1:5){
  for(fast in 1:3){

SPdata <- SPdata_orig

# calculate exponential moving averages for low pass filter trade signals
slow_lag <- slow * 30 # 150/15 generates $2,303,931.25
fast_lag <- fast * 15  # 325/85 yields $12,551,818.75
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

start_value <- 1e6
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

# return calculation
end_value <- sum(trades$pnl)
ratio <- end_value/ start_value
start_date <- min(SPdata$date)
end_date <- max(SPdata$date)
date_range <- as.numeric(difftime(end_date, start_date, units = "days")) / 365.25
ICAGR <- if(ratio <= 0) 0 else log(ratio)/ date_range
steamroller[slow, fast] <- ICAGR
steampnl[slow, fast] <- sum(trades$pnl)

trades$retrace <- 0
for (i in 1:nrow(trades)) {
  filtered_data <- SPdata %>%
    filter(date >= trades$buy_date[i] & date <= trades$sell_date[i])

  if (nrow(filtered_data) > 0) {
    trades$retrace[i] <- min(filtered_data$low)
  }
}


  }
}

print(sum(trades$pnl))