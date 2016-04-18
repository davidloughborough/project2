library(ws.data)
library(dplyr)
library(lubridate)
library(zoo)
library(RcppRoll)
library(ggplot2)
library(reshape)
```

```{r}
data(secref)
data(yearly)
data(daily.1998)
data(daily.1999)
data(daily.2000)
data(daily.2001)
data(daily.2002)
data(daily.2003)
data(daily.2004)
data(daily.2005)
data(daily.2006)
data(daily.2007)

all_daily <-
  rbind(
    daily.1998,
    daily.1999,
    daily.2000,
    daily.2000,
    daily.2001,
    daily.2002,
    daily.2003,
    daily.2004,
    daily.2005,
    daily.2006,
    daily.2007) %>%
  mutate(year = as.numeric(format(v.date,'%Y')))

daily_yearly <- left_join(all_daily, yearly, by = c("id", "year", "symbol"))

all_ws.data <- left_join(daily_yearly, secref, by = c("id", "symbol"))

cleaned_data <- all_ws.data %>%
  filter(m.ind != "REITS", price.unadj >= 1, nchar(symbol, type = "chars") <= 3)

monthly_returns1<- cleaned_data %>%
  mutate(monthYear = as.Date(as.yearmon(v.date))) %>%
  group_by(monthYear, symbol) %>%
  summarize(monthly_returns = sum(tret))

monthly_TV1 <- cleaned_data %>%
  mutate(monthYear = as.Date(as.yearmon(v.date))) %>%
  mutate(outstanding_shares = cap.usd/price.unadj) %>%
  mutate(daily_turnover = volume / outstanding_shares) %>%
  group_by(monthYear, symbol) %>%
  summarize(monthly_TV = mean(daily_turnover))

left_join(monthly_TV1, monthly_returns1, by = c("symbol", "monthYear")) -> data

prior_returns <- function(data_, months){
  data_ %>%
    group_by(symbol) %>%
    mutate(returns = roll_mean(monthly_returns, months, fill = NA, align = "right")) %>%
    ungroup() %>%
    group_by(monthYear) %>%
    mutate(rank = ntile(returns, 10)) %>%
    rename(c(
      returns = paste0(months, "_month_prior_returns"),
      rank = paste0(months, "_J_return_rank")
    ))
}

for(i in c(3, 6, 9, 12)){
  prior_returns(data, i) -> data
}

prior_TV <- function(data_, months){
  data_ %>%
    group_by(symbol) %>%
    mutate(mean_TV = roll_mean(monthly_TV, months, fill = NA, align = "right")) %>%
    ungroup() %>%
    group_by(monthYear) %>%
    mutate(rank = ntile(mean_TV, 3)) %>%
    rename(c(
      mean_TV = paste0(months, "_month_prior_TV"),
      rank = paste0(months, "_J_TV_rank")
    ))
}
for(i in c(3, 6, 9, 12)){
  prior_TV(data, i) -> data
}


future_returns <- function(data_, months){
  data_ %>%
    group_by(symbol) %>%
    mutate(returns = roll_mean(monthly_returns, months, fill = NA, align = "left")) %>%
    rename(c(returns = paste0(months, "_month_future_returns")))
}

for(i in c(3, 6, 9, 12)){
  future_returns(data, i) -> data
}

J3_data <- data %>%
  na.omit(data)  %>%
  group_by(`3_J_return_rank`) %>%
  mutate(j = 3) %>%
  summarize_each(funs(mean), `3_month_future_returns`, `6_month_future_returns`, `9_month_future_returns`, `12_month_future_returns`, `j`) %>%
  rename(c(`3_J_return_rank` = "rank"))

J6_data <- data %>%
  na.omit(data)  %>%
  group_by(`6_J_return_rank`) %>%
  mutate(j = 6) %>%
  summarize_each(funs(mean), `3_month_future_returns`, `6_month_future_returns`, `9_month_future_returns`, `12_month_future_returns`, `j`) %>%
  rename(c(`6_J_return_rank` = "rank"))

J9_data <- data %>%
  na.omit(data)  %>%
  group_by(`9_J_return_rank`) %>%
  mutate(j = 9) %>%
  summarize_each(funs(mean), `3_month_future_returns`, `6_month_future_returns`, `9_month_future_returns`, `12_month_future_returns`, `j`) %>%
  rename(c(`9_J_return_rank` = "rank"))

J12_data <- data %>%
  na.omit(data)  %>%
  group_by(`12_J_return_rank`) %>%
  mutate(j = 12) %>%
  summarize_each(funs(mean), `3_month_future_returns`, `6_month_future_returns`, `9_month_future_returns`, `12_month_future_returns`, `j`) %>%
  rename(c(`12_J_return_rank` = "rank"))

table <- rbind(J3_data, J6_data, J9_data, J12_data) %>%
  filter(rank == 1 | rank ==  5| rank  == 10)
