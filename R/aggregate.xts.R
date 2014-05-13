aggregate.xts <-
function(ser, 
                          dt, 
                          fun=mean)  # use SUM if values are 
{  
  if(!is.zoo(ser)){stop("Time series input required")}
  ser.start <- start(ser) 
  ser.end <- end(ser)
  #  qobs <- subset.zoo(ser, start, end)
  # frequency of the input series
  dt.ser <- 1/frequency(ser)/3600
  n.aggr <- dt/dt.ser
  if(length(n.aggr)>0)
  {
    if(n.aggr>1)
    {
      # aggregation (e.g quarter hour to hourly)
      tms <- seq(ser.start, ser.end, by=dt*3600)
      index.agg <- rep(tms, each=n.aggr)  # index(proj$rain)
      # trim - why needed ?
      index.agg <- index.agg[1:nrow(ser)]
      # aggregate using mean by default
      ser.agg <- aggregate(zoo(ser), by = index.agg, FUN=fun)
      names(ser.agg)<-names(ser)  
      return(ser.agg)   
    }
    else if(n.aggr<1)
    {
      return(disaggregate.xts(ser, dt))
    }
  }
  return(ser)
}
