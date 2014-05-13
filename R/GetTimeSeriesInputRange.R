GetTimeSeriesInputRange <-
function(series, start, endt, 
                                    cap="rainfall", cols=1:ncol(series),  
                                    dt=1, verbose=TRUE)
{
  if(is.null(series)){return(series)}
  #if(length(series)==0){browser()}
	# subset of columns if specified
	series <- series[,cols]
	res <- series[index(series)>=start & index(series)<=endt]
	nas <- which(is.na(res))
	if(length(nas)>0)
	{
		if(verbose==TRUE)
		{
			cat(paste(length(nas), " ", cap, " NA data encountered: replaced with 0\n"))
		}
		res[nas] <- 0
	}

	if(verbose==TRUE)
	{
	  # Determine if this value is in mm or m by looking at magnitude
	  # 1m of rainfall or pe is impossible so divide through by 1000!  
	  if(max(res,na.rm=TRUE)>10)
	  {	    
	    message("Large values encountered in series: are data in mm/hr?")
	    #res <- res /1000
	  }
    msg <- paste("Using ", length(res), " ", cap, " observations from ", start, " to ", endt, sep="")
		cat(msg, "\n")
	}
	return(res)
}
