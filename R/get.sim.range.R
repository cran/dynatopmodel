get.sim.range <-
function(proj)
{
#	require(intervals)
	rain <- proj$obs$rain
	pe <- proj$obs$pe
	qobs <- proj$obs$qobs
	try(proj$sim.start <- as.POSIXct(proj$sim.start), silent=T)
	try(proj$sim.end <- as.POSIXct(proj$sim.end), silent=T)
	
	if(length(proj$sim.start)==0)
	{
		s1 <- NA
		s2 <- NA
		s3 <- NA
		try(s1 <- start(rain), silent=T)
		try(s2 <- start(pe), silent=T)
		try(s3 <- start(qobs), silent=T)		
		
		proj$sim.start <- max(c(s1, s2, s3), na.rm=T)	
		if(!is.null(proj$sim.start))
		{
			message(paste("Start of simulation inferred from input as ", proj$sim.start))
		}
	}
	if(length(proj$sim.end)==0)
	{
		e1 <- NA
		e2 <- NA
		e3 <- NA
		try(e1 <- end(rain), silent=T)
		try(e2 <- end(pe), silent=T)
		try(e3 <- end(qobs), silent=T)
		proj$sim.end <- min(c(e1, e2, e3), na.rm=T)		
		if(!is.null(proj$sim.end))
		{
			message(paste("End of simulation inferred from input as ", proj$sim.end))
		}	
		if(proj$sim.end < proj$sim.start)
		{
			stop("Error: sim.start after end. Check supplied data and values")
		}
	}
	return(proj)
}
