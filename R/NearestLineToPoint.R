NearestLineToPoint <-
function(spLines, pt)
{
	# identify distance of near point from line to point

	mind <- gDistance(spLines, 
					  SpatialPoints(GetCoords(pt), CRS(projection(spLines)))
					  )
	#determine the line at this distance
	nrLine <- sapply(spLines@lines, 
				   function(l)
				   {
				   	sapply(l@Lines,				
				   		   function(l)
				   		   {
				   		   	lpt <- maptools::nearestPointOnLine(GetCoords(l),GetCoords(pt))
				   		   	d <- distancePts(lpt, pt)
				   		   	if(abs(d-mind)<= 1e-4){return(l)}
				   		   }
				   	)
				   	
				   }
	)	
	ret <- delete.NULLs(nrLine)
	if(length(ret) == 0){stop("Unexpected result in NearestLineToPoint")}
	return(ret[[1]])
}
