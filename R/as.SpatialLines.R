as.SpatialLines <-
function(lines, crs=CRS(NULL))
{
	return(SpatialLines(list(lines), crs))
	
}
