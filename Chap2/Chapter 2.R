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

#one symbol has bad data for six months.  It looks like the NAV is doubled. Fix.
prices <- prices %>% mutate(price=ifelse((symbol=="OLTYX" & price > 6),price/2,price))

#create return series
returns <- prices %>% transmute(date=date,return=log(price)/lag(log(price))-1) %>% slice(-1)

returns

#plot distributions
# remove OIMYX OMFL since they don't have long histories
returns %>% filter(symbol != "OMFL" &  
                    symbol != "OIMYX" &
                     symbol != "ESGF") %>% 
  ggplot(aes(return,fill=symbol))+geom_density(alpha=0.5)

returns %>% filter(symbol != "OMFL" &  
                     symbol != "OIMYX" &
                     symbol != "ESGF") %>% 
  ggplot(aes(return,fill=symbol))+geom_histogram()+facet_wrap(~symbol)

#-----------------------------
#BONUS Cumulative Return "Mountain Chart" function
chart_cum_returns<-function(Ra, 
                            ret_col='return',
                            name_col='name',
                            date_col='date',
                            shape=c('long','wide')){
  #Returns a ggplot2 line chart object for plotting or further annotation
  #Crucially, this function adds a dummy date at the beginning of the series to start.
  #the plot at the origin without a gap.
  Ra<-ungroup(Ra)
  Ra<-select(Ra,name = name_col,return=ret_col,date=date_col)
  shape<-match.arg(shape)
  if (shape=='wide'){
    Ra<- Ra %>%
      gather(name,return,-date)
  } else{
    Ra<- Ra %>% select(date,name,return)
  }
  
  #create wealth index. Add a date to start at "1"
  # that is equal to the length between the first and second dates
  new_dates<-(Ra$date[1]-as.numeric(Ra$date[2]-Ra$date[1])) %>% c(Ra$date)
  Ra<- Ra %>% 
    group_by(name)%>% 
    complete(date=new_dates,fill=list(return=0)) %>% 
    mutate(wealth=cumprod(1+return))
  
  gg<-Ra %>% 
    as.tibble() %>% 
    group_by(name) %>% 
    ggplot(aes(x=date,y=wealth,color=name))+geom_line()
  
  return(gg) 
}
#-----------------------------
returns %>% 
#  filter(symbol==c("OMFL")) %>%
  chart_cum_returns(name_col="symbol",shape="long")  
