approx.pe.ts <-
function(start, end, 
                           dt=1,  # time step in *hours*
                           emin=0, 
                           emax=10/1000)   # annula maximum daily total (typically m or mm, be careful)
{
  # times must be in local times e.g. GMT
  # POSIXlt (rather than POSIXct) comprises the necessary info to parse
  # date into year number etc  
#  CheckClass(start, expected = "POSIXlt", stop=TRUE)
#  CheckClass(end, expected="POSIXlt", stop=TRUE)
  start <- as.POSIXlt(start)  # allowsd us to use yday etc
  end <- as.POSIXlt(end)
  # dt in hours
  # day of year number 
  dStart <- start$yday
  # require whole number of days, so adjust
  start$hour <- 0
  
  end$hour <- 0
  
  nDays <- round((end-start)[[1]]+1)
  vals <- pEvap(dt, dStart=dStart, nDays, emin, emax)
  # step is in hours = 3600s
  times <- seq(from=start, length.out=length(vals), by=dt*3600)
  len <- min(length(vals), length(times))
  
  times <- times[1:len]
  
  res <- xts(vals[1:len], order.by=times)
  return(res) 
}
