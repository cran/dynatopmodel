ReadDischargeData <-
function(fn, dt=1, start=NULL, end=NULL, 
							  aggregate =F, 
							  headerlines=14)
{
	flow <- read.csv(fn, header=F, skip= headerlines)
	# remove index column if it exists
	#   if(ncol(flow)>2)
	#   {
	#     flow <- flow[,-1]
	#   }
	# "unfactor" the first column
	flow[,1] <- as.character(flow[,1])
	
	# parse the first col into dates - should be in UTC format e.g. 1969-11-21T22:00:00
	# see http://www.cl.cam.ac.uk/~mgk25/iso-time.html
	#datetimes<- strptime(flow[,1],"%Y-%m-%d",tz="GMT")
	datetimes<- strptime(flow[,1],"%Y-%m-%dT%H:%M:%S")  #,tz="GMT")
	#   inperiod <- which(datetimes >= start & datetimes <= end)
	#   # select subset
	#   datetimes <- datetimes[inperiod]
	vals <- flow[,2]
	names(flow) <- c("date", "flow")
	
	cat(nrow(flow), " records loaded... creating time series...\n")
	
	#vals <- which(date)
	# create a time series (daily mean flow in m^3/hr)
	sel<-paste(start, "::", end, sep="")
	Qobs <- xts(vals,order.by=datetimes)
	
	if(!is.null(sel)){Qobs <- Qobs[sel]}
	
	if(aggregate)
	{
		# convert to aggregated hourly data (means)
		l <- 3600*dt
		cat("aggregating to every", dt, " hour(s)...\n")
		Qobs <- aggregate(Qobs, time(Qobs) - as.numeric(time(Qobs)) %% l, mean)}
	return(xts(Qobs))
}
