library(xts)
library(lubridate)
library(quantmod)
library(tidyverse)
library(tibbletime)
library(timetk)

YEARS_BACK <- 5
symbols <-
  c("OLTYX",
    "OEGYX",
    "RDIV",
    "RMUYX",
    "ESGF",
    "OMFL",
    "OIMYX",
    "OIGYX",
    "GLVYX",
    "MIGYX")

raw_symbols <-
  getSymbols(
    symbols,
    src = 'yahoo',
    from = Sys.Date() - dyears(5),
    to = Sys.Date(),
    auto.assign = TRUE,
    warnings = TRUE
  )

raw_prices <- raw_symbols %>%
  map( ~ Ad(get(.))) %>%
  reduce(merge) %>%
  `colnames<-`(symbols)

# BAD drops incomplete rows with NAs
to.monthly(raw_prices,indexAt = "lastof",OHLC=FALSE)

#SO go right to tidyverse and time-aware tibble
prices<-timetk::tk_tbl(raw_prices) %>%
  rename(date=index) %>% 
  tibbletime::as_tbl_time(index=date) 

#monthlyize
prices <- prices %>% tibbletime::as_period("monthly",side="end")

#tidyfy it
prices <- prices %>% gather(symbol,price,-date) %>% group_by(symbol)
#create return series
returns <- prices %>% transmute(date=date,return=log(price)/lag(log(price))-1) %>% slice(-1)
returns

  