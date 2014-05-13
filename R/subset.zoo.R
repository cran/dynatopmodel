subset.zoo <-
function(obj, start, end)
{
  start <- as.POSIXct(start)
  end <- as.POSIXct(end)
# 	require(intervals)
#   int1 <- 
#   int2 <- Intervals(c(start, end))
	if(!is.zoo(obj))
	{
		warning("Non-zoo object supplied to subset.zoo")
		return(obj)		
	}
  set1 <- which(index(obj) >= start)
  set2 <- which(index(obj) < end)
#  if(length(set1)==0){return(obj[])}
  ind <- intersect(set1,set2)
  return(obj[ind])
}
