#utility functions
#---------------------------------------------------------
#utility function to subset tidy return time series by year a certain year back
#filter out incomplete series
subset_year <- function(tidy_ret,yr){
  tidy_ret %>% group_by(symbol) %>% 
    filter(year(date)>=yr) %>% 
    #filter out incomplete series
    filter(!is.na(sum(return)))
}
#---------------------------------------------------------
#utility function to get volatility
volatility<-function(x,n=250){
  return(sd(x)*(n^.5)*100)
}
#---------------------------------------------------------
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
#---------------------------------------------------------