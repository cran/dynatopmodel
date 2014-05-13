spLineInvert <-
function(lns)
{
	if(!is.projected(lns)){stop("Projected coordinates system required")}
	crs <- CRS(projection(lns))
	ID 	<- lns@lines[[1]]@ID
	coords <- matrix(unlist(coordinates(lns)), ncol=2)
	# invert
	coords <- coords[rev(row(coords)[,1]),]
	SpatialLines(list(Lines(list(Line(coords)), ID=ID)), proj4string=crs)		
}
