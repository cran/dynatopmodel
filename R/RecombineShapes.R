RecombineShapes <-
function(shps)
{
	res <- shps[[1]]
	for(i in 1:length(shps))
	{
		shp <- shps[[i]]
		cat("combining shape id ", GetLineID(shp), "\n")
		res<-gUnion(res, shp, byid=T)
		
	}
	return(res)	
}
