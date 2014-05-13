allocate.rain <-
function(rain, groups, it)
{
	# can index by time instead of integer (performance implications?)
	if(is.datetime(it)){it <- which(index(rain)==it)}
# 	if(all(colnames(rain)==groups$id))
# 	{
# 		# already split the rainfall input up by response unit
# 		rain.dist <- rain[it,]   
# 	}
# 	else
# 	{	
# 	
# 	  if(it>nrow(rain))
# 	  {
# 	    warning("Rainfall record truncated at ", tail(index(rain), 1))
# 	    it <- nrow(rain)
# 	  }
# 	
	#ncol <- ncol(rain)
	# gauge id, limited to nunber of data columns	
	rain.dist <- rain[it,pmin(groups$gauge.id, ncol(rain))]   
	
  return(as.vector(rain.dist))
}
