spLinesTranslate <-
function(lns, x, y)
{
#	if(!is.projected(lns)){stop("Projected coordinates system required")}
#	crs <- CRS(projection(lns))
	ID 	<- lns@ID

	ll <- lapply(coordinates(lns), 
				 function(coords)
				 {
				 	coords <- cbind(coords[,1]+x, coords[,2]+y)
				 	Line(coords)
				 }
	)
	# shift	
	Lines(ll, ID=ID)
}
