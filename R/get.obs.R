get.obs <-
function(rain, sim.start, sim.end=NULL, dt=1)  # dt is in hours
{
	if(length(rain)==0)
	{
		# must have both limits if nothing supplied here
		tms <- seq(sim.start, sim.end, by=dt*3600)
		message("Time series input created ", obj.name(rain))		
		rain <- xts(rep(0, length(tms)), order.by=tms)   #  could attempt to 
	}
	else if(is.zoo(rain))
	{
		#rain <- rain	
	}
	# if arrays without time info supplied attamept to create time series
	else if(is.matrix(rain) & !is.null(sim.start))
	{
		tms <- seq(sim.start, length.out=nrow(rain), by=dt)
		rain <- xts(rain, order.by=tms)   
		
	}
	else if(is.vector(rain) & !is.null(sim.start))
	{
		tms <- seq(sim.start, length.out=length(rain), by=dt)		
		rain <- xts(rain, order.by=tms)   				
	}	
	# fix any NAs - warn if this
	which.na <- which(is.na(rain[])) 
	if(length(which.na)>0)
	{
		warning(paste(length(which.na), " NA observations replaced by zeroes"))
		rain[which.na] <-0	 # deals with multi dimensional obeject
	}
	
	return(rain)
}
