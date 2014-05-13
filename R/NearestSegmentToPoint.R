NearestSegmentToPoint <-
function(line, pt)
{
	lcoords <- GetCoords(line)
	pcoords <- GetCoords(pt)
	
	mindseg <- NULL
	mind <- 1e8
	for(segNo in 1:(nrow(lcoords)-1))
	{
		seg <- lcoords[segNo:(segNo+1),]
		nrpt <- maptools::nearestPointOnSegment(seg,pcoords)
# 		if(all(nrpt[1:2]==pcoords))
# 		{
# 			return(seg)
# 		}
		d <- nrpt[3] # third element is the distance
		if(d <mind)
		{
			mind <- d
			mindseg<-seg
		}	
	}
	
	# return nearest - point not on line
	return(mindseg)
	
}
