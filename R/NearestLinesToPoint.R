NearestLinesToPoint <-
function(spLines, pt)
{
	# identify distance of near point from line to point
	crs <- CRS(projection(spLines))
	spPt <- SpatialPoints(GetCoords(pt), crs)
	mind <- gDistance(spLines, spPt)
	# ID
	#determine the Lines that approach the point at this distance
	nrLines <- sapply(spLines@lines, 
					 function(l)
					 {
					 	d <- gDistance(as.SpatialLines1(l, crs), spPt)
					 	if(abs(d-mind)<= 1e-5){return(l)}					 	
					 }
	)	
	ret <- delete.NULLs(nrLines)
	if(length(ret) == 0){stop("Unexpected result in NearestLinesToPoint")}
	return(ret[[1]])
}
