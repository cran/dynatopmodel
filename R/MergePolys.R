MergePolys <-
function(parent, child, ID=parent@polygons[[1]]@ID)
{
	#	if(!is.projected(from)){stop("Projected coordinates system required")}
	crs <- CRS(projection(parent))
	
	ID 	<- parent@polygons[[1]]@ID
	coords1 <- matrix(unlist(coordinates(parent)), ncol=2)
	coords2 <- matrix(unlist(coordinates(child)), ncol=2)
	
	coords <- rbind(coords1, coords2)
	SpatialPolygons(list(Polygons(list(Polygon(coords)), ID=ID)), proj4string=crs)	
	
}
