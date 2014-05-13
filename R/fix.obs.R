fix.obs <-
function(rain, maxgaps=24, missing.data=NA)
{
  bad <- which(is.na(rain))
  if(length(bad)>0)
  {
  	# if a non-na value  missing.data will replace the gaps
  	rain[bad]<- missing.data
	  nbad <- length(bad)
	  cat(nbad, " missing rainfall data... attempting interpolation...")
	    
	  # linear interpolation
	  rain <- na.approx(rain, maxgap=maxgaps)
	  
	  # mark interpolated data
#	  rain[bad]$status <- -1  #"I"
	    
	  nbad2 <- length(which(is.na(rain)))
	  nfixed <- nbad - nbad2
	  
	  cat(nbad2, " missing data remaining\n")
	    
	  message(nfixed, " missing data interpolated, ", nbad2, " remaining")
  }
  return(rain) 
}
