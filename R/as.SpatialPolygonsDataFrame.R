as.SpatialPolygonsDataFrame <-
function(spl)
{  
	crs <- GetCRS(spl)

	res<-list()
	
	for(i in 1:length(spl))
	{	
		res <- c(res, SpatialLines(spl[i]@lines[1], crs))		
	}
	return(res)
}
