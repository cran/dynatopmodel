DistanceMatrix <-
function(pts1, pts2)
{
	dists <- matrix(nrow=length(pts1),ncol=length(pts2))
	for(i1 in 1:length(pts1))
	{
		for(i2 in 1:length(pts2))
		{
			dists[i1, i2] <- gDistance(pts1[i1], pts2[i2])				
		}
	}
	return(dists)
}
