library(xts)
library(lubridate)
library(quantmod)
library(tidyverse)
library(tibbletime)

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




