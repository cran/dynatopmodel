LineToIntersection <-
function(l, pt)
{
	# need to expand the point into a polygon otherwsie gIntersection doesn't work
	pt<- gBuffer(pt)
	if(!gIntersects(l, pt))
	{
		# warn if point wasn't found
		warning(paste("Point ", coordinates(pt), " not found on line"))
		return(l)
		
	}
	coords <- matrix(unlist(coordinates(l)), ncol=2)
	ID <- GetLineID(l)
	crs <- CRS(projection(l))	
	found <- F
	iseg<-1
	while(!found & iseg < nrow(coords))
	{		
		seg <- SpatialLines(list(Lines(list(Line(coords[iseg:(iseg+1), ])), ID="1")), 
							proj4string=crs)
		
		if(gIntersects(seg, pt))
		{
			found <- T
		}
		else
		{		
			iseg <- iseg+1
		}
	}

	ncoords <- rbind(coords[1:iseg, ], coordinates(pt))
	
	return(SpatialLines(list(Lines(list(Line(ncoords)), ID=ID)), 
					   proj4string=crs))
	
}
