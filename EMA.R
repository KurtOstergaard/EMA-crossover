#  long-only exponential moving average crossover trading system from Ed Seykota
rm(list=ls())
library(tidyverse)
library(lubridate)
library(tidyquant)
library(plotly)
library(Rcpp)
library(profvis)
library(rlang)
library(ggrepel)

options(ggrepel.max.overlaps = Inf)      # ggrepel options for ggplot2
theme_set(theme_light())                # ggplot theme or _bw()

start_time <- Sys.time()
# C code for EMA calculation with no leading NA
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
df <- read_csv("SP----C.csv",
     col_names = c("date", "open", "high", "low", "close", "volume", "open_int"),
     col_types = list(
       date = col_date(format = "%Y%m%d"),
       open = "d",
       high = "d",
       low = "d",
       close = "d",
       volume = "d",
       open_int = "d")
     )

# calculate ATR, average true range, for risk sizing the trades
df <- df |>
  select(date:close) |>
  mutate(hc_yest = high - lag(close),
         lc_yest = lag(close) - low,
         range = high - low) |>
  mutate(ATR = pmax(range, hc_yest, lc_yest, na.rm = TRUE))
df$atr_EMA <- ewmaRcpp(df$ATR, 20)
df_orig <- df

# profvis({

# optimization section
runs <- expand.grid(fast = seq(15, 85, 70), slow = seq(150, 325, 175))
#  seq(20, 400, 10), seq(100, 800, 20)) # >10 min   70/360 =>$13M, 11% ICAGR!
# future run seq(15, 150, 5) seq(85, 360, 5) ~10+ mins
results <- tibble() # vector(mode = "list", length = nrow(runs))
colnames(results) <- unlist(str_split("j, slow_lag, fast_lag,
            ICAGR, drawdown, bliss, lake, end_val, trade_test", ", "))

for (j in seq_len(nrow(runs))) {
  df <- df_orig
  # calculate exponential moving averages for low pass filter trade signals
  slow_lag <-  runs$slow[j]  # 15/150 ends with $3,303,931.25
  fast_lag <-  runs$fast[j]  # 85/325 leads to $13,551,818.75
  df$slow <- ewmaRcpp(df$close, slow_lag)
  df$fast <- ewmaRcpp(df$close, fast_lag)

  df <- df |>           # EMA crossover
    mutate(cross = fast - slow,
           on = ifelse(cross > 0 & lag(cross) < 0, 1, 0),
           off = ifelse(cross < 0 & lag(cross) > 0, -1, 0))
  df <-  slice(df, 26:n()) # drop first 25 rows to 'warm up' EMA

  if(df$cross[1] > 0) df$on[1] <- 1    # trade signal details
  df <- df |>
    mutate( signal = on + off,
            buy_date = ifelse(on == 1, as_date(lead(date)), 0),
            buy_price = ifelse(on == 1, (lead(open) + lead(high))/2, 0),
            buy_amount = 0,
            sell_date = ifelse(off == -1, as_date(lead(date)), 0),
            sell_price = ifelse(off == -1, (lead(open) + lead(low))/2, 0))

  # Close out last trade if long when data file runs out
  if (df$cross[nrow(df)] > 0) {
    df$off[nrow(df)] <- -1
    df$signal[nrow(df)] <- -1
    df$sell_date[nrow(df)] <- df$date[nrow(df)]
    df$sell_price[nrow(df)] <- df$close[nrow(df)]
  }

  # set up pnl & equity columns
  start_value <- 1e6 ; df$closed_pnl <- 1e6 ; df$highwater <- 1e6
  df$open_pnl <- 0 ; df$equity <- 0 ; df$water <- 0
  df$drawdown <- 0 ; buy_amount <- 0 ; buy_price <- 0
  heat <- 0.1  # aka risk budget, is 10% of the book equity for each trade
  ATR_multiplier <- 5 # dynamic risk sizing based on current volatility via ATR

  # calculate trade pnl i=1
  if(df$signal[1] == 1){
    buy_amount = plyr::round_any((df$closed_pnl[[1]] *
                heat) / (ATR_multiplier * df$atr_EMA[[1]]), 250, f = round)
    df$buy_amount[1] = buy_amount
    buy_price = df$buy_price[1]
  }
  # calculate trade pnl i>1
  for (i in seq2(2, nrow(df))){
    df$closed_pnl[i] = df$closed_pnl[i-1]
    if(df$signal[i] == -1){
      df$closed_pnl[i] = df$closed_pnl[i] +
        (df$sell_price[i] - buy_price) * buy_amount
      buy_amount = 0
      df$open_pnl[i] = 0
    } else if(df$signal[i] == 1){
      buy_amount = plyr::round_any((df$closed_pnl[[i]] *
              heat) / (ATR_multiplier * df$atr_EMA[[i]]), 250, f = round)
      df$buy_amount[i] = buy_amount
      buy_price = df$buy_price[i]
    } else {
      df$open_pnl[i] = (df$close[i] - buy_price) * buy_amount
    }
    df$equity[i] <- df$open_pnl[i] + df$closed_pnl[i]
    df$highwater[i] <- pmax(df$equity[i], df$highwater[i-1])
    df$water[i] <- df$highwater[i] - df$equity[i]
    df$drawdown[i] <- df$water[i] / df$highwater[i]
  }

  # make the summary trade table
  trade_test <- sum(df$signal)
  if(trade_test == 0) {
    buys <- df |>
      select(on, buy_date, buy_price, buy_amount) |>
      filter(on == 1) |>
      select(!on)
    sells <- df |>
      select(off, sell_date:drawdown) |>
      filter(off == -1) |>
      select(!off)
    trades <- bind_cols(buys, sells)
    trades$buy_date <- as.Date(as.numeric(trades$buy_date))
    trades$sell_date <- as.Date(as.numeric(trades$sell_date))
  }

  # return calculation
  end_value <- df$equity[nrow(df)] ; end_val <- end_value / 1000000
  ratio <- end_value/ start_value
  start_date <- min(df$date) ; end_date <- max(df$date)
  date_range <- as.numeric(difftime(end_date, start_date, units = "days")) / 365.25
  ICAGR <- if(ratio <= 0) 0 else log(ratio)/ date_range
  drawdown <- max(df$drawdown)
  lake <- sum(df$water) / sum(df$equity)
  bliss <- ICAGR / drawdown
  results[j,1:9] <- as_tibble_row(
    c(j=j, slow_lag=slow_lag, fast_lag=fast_lag, ICAGR=ICAGR, drawdown=drawdown,
    bliss=bliss, lake=lake, end_val=end_val, trade_test=trade_test),
    .name_repair = "universal")

}       # optimization loop end
# }) # profvis

end_time <- Sys.time() ;forever <- end_time - start_time
secs <- forever  / nrow(runs)
sprintf("Yo, %1.2f total time and %1.4f per run, %i runs", forever, secs, nrow(runs))

# the promised land of pretty graphs
df |>        # S&P
  ggplot(aes(x = date, y = close)) +
  geom_line(linewidth = 1, alpha = 0.5)

ggplot() +
  geom_point(data=df, aes(x=date, y=close), alpha=0.2) +
  geom_segment(data=trades, aes(x=buy_date, y=buy_price, xend=sell_date,
              yend=sell_price, size = 1, color="black"))

df |>        # lake
  ggplot(aes(x = date)) +
  geom_ribbon(aes(ymin=equity, ymax=highwater, x=date, fill = "band"), alpha = 0.9)+
  scale_color_manual("", values="grey12")+
  scale_fill_manual("", values="red")

df |>       # equity and highwater
  mutate(close_big = close*10500) |>
  ggplot(aes(x = date)) +
  geom_line(aes(y = equity),size = 1,  alpha = 0.9) +
  geom_point(aes(y=highwater), size=1, shape = 4, alpha=0.1, color="black") +
  geom_line(aes(y=close_big), size=1, alpha=0.2)

# check for second scale
# https://stackoverflow.com/questions/59956953/plotting-secondary-axis-using-ggplot

df |>
  ggplot(aes(x = date)) +
  geom_line(aes(y = equity), size = 1, alpha = 0.8) +
  geom_line(aes(y = highwater), size=1, alpha=0.2)

rzlt <- results|>
  filter( slow_lag > fast_lag)

rzlt |>
  ggplot(aes(x = ICAGR, y = drawdown)) +
  geom_point(size = 3, shape = 4)

rzlt |>
  ggplot(aes(x = lake, y = bliss)) +
  geom_point(size = 3, shape = 4)

rzlt |>
  ggplot(aes(x = lake, y = drawdown)) +
  geom_point(size = 3, shape = 4)

#  geom_smooth(method = "lm")

rzlt |>
  ggplot(aes(x = ICAGR, y = lake)) +
  geom_point(size = 3, shape = 4)

rzlt |>
  ggplot(aes(x = ICAGR, y = bliss)) +
  geom_point(size = 3, shape = 4)

pnl <- rzlt |>
  select(slow_lag, fast_lag, end_val) |>
  pivot_wider(names_from = slow_lag, values_from = end_val) |>
  as.matrix()
blip <-  sort(unique(pnl[,1]))
rownames(pnl) <- blip
pnl <- pnl[,-1]


plot_ly(z = ~pnl) |> add_surface(
  contours = list(
    z = list(
    show=TRUE,
    usecolormap=TRUE,
    highlightcolor="#ff0000",
    project=list(z=TRUE)
    )
  )
)

happy <- rzlt |>
  select(slow_lag, fast_lag, bliss) |>
  pivot_wider(names_from = slow_lag, values_from = bliss) |>
  as.matrix()
happy <- happy[,-1]
xax <- sort(unique(rzlt$slow_lag))
yax <- sort(unique(rzlt$fast_lag))

plot_ly(z = happy, x=xax, y=yax, type = "surface") |>
  layout(scene = list(
    xaxis=list(title="slow"),
    yaxis=list(title="fast", autorange="reversed"),
    zaxis=list(title="Bliss")
  ))


fig <- plot_ly(z = happy, x=xax, y=yax) |> add_surface(
  contours = list(
    z = list(
      show=TRUE,
      usecolormap=TRUE,
      highlightcolor="#ff0000",
      project=list(z=TRUE)
    )
  )
)
fig <- fig |>  layout(
  scene = list(
    xaxis=list(title="slow"),
    yaxis=list(title="fast", autorange="reversed"),
    zaxis=list(title="Bliss")
  ))

fig


plot_ly(z = ~happy) |> add_surface(
  contours = list(
    z = list(
      show=TRUE,
      usecolormap=TRUE,
      highlightcolor="#ff0000",
      project=list(z=TRUE)
    )
  )
)

cc <-  cor(rzlt$ICAGR, rzlt$drawdown, method="pearson")
cc
plot_ly(z = volcano, type = "surface")

# https://win-vector.com/2015/07/27/efficient-accumulation-in-r/
# notes on efficient accumulation

trades |>       # trades
  ggplot(aes(x = date)) +
  geom_segment(aes(x=buy_date, y=buy_price, xend=sell_date, yend=sell_price,
                   color="black"))

trades |>       # trades
  ggplot(aes(x = date)) +
  geom_segment(aes(x=buy_date, y=buy_price, xend=sell_date, yend=sell_price,
                   color="black"))
