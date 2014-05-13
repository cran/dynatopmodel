MergeLines <-
function(parent, child, ID=parent@lines[[1]]@ID)
{
#	if(!is.projected(from)){stop("Projected coordinates system required")}
	crs <- CRS(projection(parent))
	
	ID 	<- parent@lines[[1]]@ID
	coords1 <- matrix(unlist(coordinates(parent)), ncol=2)
	coords2 <- matrix(unlist(coordinates(child)), ncol=2)

	coords <- rbind(coords1, coords2)
	SpatialLines(list(Lines(list(Line(coords)), ID=ID)), proj4string=crs)	
	
}
