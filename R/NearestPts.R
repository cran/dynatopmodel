NearestPts <-
function(pts1, pts2, nres=1)
{
	dists <- DistanceMatrix(pts1, pts2)
	nrow <- nrow(dists)	
	distords <- order(dists[])
	if(nres>length(distords))
	{
		warning(paste("cannot return requested number of points - sets too small: ", nres))
	}
	
	npti <- distords[1:nres]
	cols <- ceiling(npti / nrow)
	rows <- npti - nrow*(cols-1)
	dpairs <- dists[npti] # checks too
	
	# return nx2 matrix with index of pairs of nearest points, the third columsn 
	return(cbind("p1"=rows, "p2"=cols, "dist"=dpairs))  # rows = p1, col=p2
}
