LineCentroid <-
function(l)
{
	coords <- matrix(unlist(coordinates(l)), ncol=2)
	return(colMeans(coords))
	
}
