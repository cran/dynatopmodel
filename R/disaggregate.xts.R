disaggregate.xts <-
function(ser, 
                             ser.start=start(ser),
                             ser.end=end(ser),
                             fact=NULL,    # factor by which to divide existing time interval e.g fact=4 converts houly intervals to quarter hours
                             dt=NULL, 
                             is.rate=F)  # IF true then each value in teh series is replicated across the finer time intervals, otherwise the initial value is averaged averaged over 
{
  # if the set is NULL then return
  if(is.null(ser)){return(ser)}
  if(!is.zoo(ser)){stop("Time series required")}
  if(is.null(fact)& is.null(dt)){stop("Supply either aggregation factor or new time interval")}
  ser <- subset.zoo(ser, ser.start, ser.end)
  tms <- as.double(index(ser))
  dt.ser <- round(mean(diff(tms)/3600,na.rm=T),2)

  # frequency of the input series to the 
  if(is.null(fact))
  {
    # disaggregation factor (nearest)
    tryCatch(fact<-ceiling(dt.ser/dt), 
             error=fact<-1)
  #  if(fact != dt.ser/dt)
   # {
   #   warning("Time interval ", dt, "h is not a divisor of original interval ", dt.ser, "h")
  #  }
  }
  if(length(fact)>0)
  {
    if(fact>1)
    {
    	# disaggregating observations from larger to smaller time interval by given factor
      # recalc interval
      dt <- dt.ser/fact
      #  times for new series 
    #  tms <- seq(ser.start, ser.end, by=dt*3600)
       
      # new values duplicate series and rescale if not a rate in terms of a fixed
      # period e.g m/hr
      vals <- apply(as.matrix(ser), MARGIN=2, FUN=rep, each=fact)    
      vals <-matrix(coredata(vals), ncol=ncol(ser))
    
      tms <- seq(ser.start, along.with=vals, by=dt*3600)
    # if the value is a rate then it should be applied to all of the values in 
    # the interval "as is". Otherwise each values needs to be divided across the 
    # smaller time steps so that the total across the original time intervals is the same
      if(!is.rate){ser.agg <- ser.agg/fact}
      ser.agg <- xts(vals, order.by=tms[1:nrow(vals)])  
      
      names(ser.agg)<-names(ser)  
      
      return(ser.agg)
    }
  }

  #  aggegrating to larger time interval and fewer observations
  return(aggregate.xts(ser, dt))  
}
