read.obs <-
function(fn, 
                          #  start=NULL, # return subset of 
                          #  end=NULL,       
                            maxgaps=24, # largest gap in data that can be interpolated
                            tm=1,   # col name, col number or vector (start, end, number is inferred from number of observations. If missing, generate a time series from start and end)
                            val=2,  # col number(s) or name(s) containing observations. if > number of colums then truncate
                            fmt="%Y-%m-%d %H:%M:%S",   # date time format string
                            tz="GMT",           #  tz defaults to UTC, blank uses current tz - watch out for daylighgt saving time changes
                            header=is.character(tm) | is.character(val),           # whether column names are present 
                            metalines=0,        # any metadata lines - can also point to a file location                             
                            units="m",          # expected units
                            dt=1,               # default time step in hours
                            sep="\t",
										        missing.data = NA,  # missing data values: set to e.g. 0 to treat as no reading
                            include.status=F)   # include an integer status code for each observation in another colums
{

#  try(header <- header.lines(fn, sep, skip=metalines)>0)
  # determine and override type

  sep.rep <- switch(tools::file_ext(fn),
         "csv"=",",
         "tsv"="\t",
         "dat"="\t")
  if(!is.null(sep.rep)){sep<-sep.rep}
  
  # is the first line a numeric
#  first <- split(first.line, split=sep)
#  try(if(is.character(first.line[[2]])){header=T})

	rain <- read.table(fn,
                     fill=T,
	                 header=header, # if using named columns then this must be TRUE
	                 sep=sep,   # or delimited if sep=","
	                 skip=metalines)    # how mant lines to skip at head
  
  if(length(tm)>0)
  {
    
    # time column specified
    # e.g. vector of start, end times 
#     times <- seq(start, end, length.out = nrow(rain)) 
	  # check for any column headers, if not found then assumme that first col is time and second the reading
	  time_str<-  rain[,tm]  #paste(rain$year,"-",rain$month,"-",rain$day," ",rain$hr,":00:00",sep="")
    if(length(tm)>1)
    {
      # collapse into a single column assuming e.g first is date and second the time      
      time_str<-do.call(paste, time_str)
    }
#     start <- rain[1, tm[]
#     end <- rain[nrow(rain), tm]    
	  # parse to local times (POSIXlt), then calendar times ()
	  times <- strptime(time_str, fmt, tz=tz)   #, tz="GMT")
  }
  else
  {
    # assume given time step (1 hr by default) and create a dummy time series
    start <- as.POSIXct("2000-01-01")
    end <- as.POSIXct(start+dt*3600*(nrow(rain)-1))
    times <- seq(start, end, by=dt*3600)  
    message(paste("Time index created for series from ", start, " to ", end, " by ", dt, "hours"))
  }  
  # cam specify all colukms byu supplying a large range of column indexes containing observations
  # will be truncated to the column count
  if(is.numeric(val)& max(val)>ncol(rain))
 	{
  	val<-min(val, ncol(rain)):ncol(rain)
  }
  nms <- colnames(rain)[val]
  # ensure numeric, char arguments will become NAs
  rain <- apply(as.matrix(rain[,val]), MARGIN=2, function(x){as.numeric(x)})
  # strip out invalid cols
  OK.cols <- apply(rain, MARGIN=2, function(x){!all(is.na(x))})
  rain <- rain[, OK.cols]
	# second column is the hourly measured rainfall. Create time series
	rain <- xts(rain, order.by=as.POSIXct(times))	
	# determine ends of period
#	if(is.null(start)){start<-times[1]}
#	if(is.null(end)){end<-times[length(times)]}
	
#	sel<-paste(start, "::", end, sep="")
  # remove data whose time entries couldn't be parsed
	badtimes <- which(is.na(times))
	if(length(badtimes)>0)
  {
	  rain <- rain[-badtimes,]
  }	
	if(length(rain)==0)
	{
    stop("No valid times located in data source - check time string format ", fmt, "\n")
	}
 # rain <- apply(rain, MARGIN=2, FUN=fix.obs, maxgaps=maxgaps, missing.data=missing.data)
  n <- nrow(rain)
  cat(paste("Read ", length(rain), " records for period ", 
      index(rain[1]), " to ",  index(rain[n]), " no. missing=", check.obs(rain), "\n"))
  
  if(units=="mm" & max(rain>1)){
    message("Data may be in mm")
  }
  colnames(rain)<-nms[OK.cols]
  # just the observations
  return(rain)
}
