distancePts <-
function(pt1, pt2)
{
	xy <- GetCoords(pt2)-GetCoords(pt1)
	return(sqrt(rowSums(xy^2)))	
	
}
