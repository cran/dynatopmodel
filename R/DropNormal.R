DropNormal <-
function(pt,line,as.vector=T)
{
	npt <- maptools::nearestPointOnLine(line, pt)
	if(as.vector)
	{
		# directed vector
		return(npt-pt)
	}
	# endpoints of normal line
	return(rbind(pt, npt))
}
