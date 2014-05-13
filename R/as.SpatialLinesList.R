as.SpatialLinesList <-
function(spl)
{  
  crs <- GetCRS(spl)
	if(CheckClass(spl, "SpatialLinesDataFrame"))
	{
		spl<-SpatialLines(spl@lines, crs)
	}
	res<-list()
	
	for(i in 1:length(spl))
	{	
		res <- c(res, SpatialLines(spl[i]@lines[1], crs))		
	}
	return(res)
}
