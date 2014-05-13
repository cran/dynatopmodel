ConnectLines <-
function(from, to, addLine=F)
{
	if(!is.projected(from)){stop("Projected coordinates system required")}
	crs <- CRS(projection(from))
	
	ID 	<- from@lines[[1]]@ID
	coords <- matrix(unlist(coordinates(from)), ncol=2)
	tocoords <- matrix(unlist(coordinates(to)), ncol=2)
	
	
	# determine nearest point
	p1 <- SpatialPoints(coords, crs)
	p2 <- SpatialPoints(tocoords, crs)
	
	if(gIntersects(from, to))
	{

		pt <- gIntersection(from, to)
		return(LineToIntersection(from, pt))

	}	
	else
	{
		nearpts <- NearestPts(p1,p2)
		
		# shouldn't happen!
		if(nrow(nearpts)==0){stop("No near points located in ConnectLines")}
		if(nrow(nearpts)>1){warning("Unexpected number of near points located in ConnectLines")}
		# insert coords of nearest point on target line in front of coords in source
		# shoudl always be at least one point pair in result set - nearest point pairs
		# usually endpoints of line segments
		tindex <- nearpts[1,1]
		sindex <- nearpts[1,2]	
		if(addLine)
		{
			# insert extra line segment
			coords <- rbind(coords[1:(tindex),], tocoords[sindex,])  #, coords[tindex:nrow(coords),])
		
		}
		else
		{
			coords <- rbind(coords[1:(tindex-1),], tocoords[sindex,])  #, coords[tindex:nrow(coords),])
			# overwrite
		#	coords <- rbind(coords[-nrow(coords), ], tocoords[1,])		
		}
	}
	SpatialLines(list(Lines(list(Line(coords)), ID=ID)), proj4string=crs)	
	
}
