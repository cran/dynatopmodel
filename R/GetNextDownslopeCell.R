GetNextDownslopeCell <-
function(rast, cur, random)
{
	# numbered 1 to 8, 
	adj <- adjacent(rast,cur,directions=8,pairs=F)  		   	
	downslope <- adj[which(rast[adj]<rast[cur])]
	ndest <- length(downslope)  
	if(ndest>0)
	{
		locn <- matrix(rep(xyFromCell(rast, cur),ndest),ncol=2, byrow=T)
		# geographical location of downslope cells
		downslopelocn <- xyFromCell(rast, downslope)
		# vector lengths to adjacent upslope cells (midpoints)
		#  	browser()
		# pythagoras
		dists <- sqrt(rowSums((downslopelocn-locn)*(downslopelocn-locn)))
		# calculate slopes (check not NaN)
		slopes <- (rast[downslope]-rast[cur])/dists 
		
		# contour lengths perp to lines joining midpoints - see Quinn et al 1991
		# these are  easily determined by their relationship with the intercells dists
		clens <- dists/3
		
		# (b) randomly select downstream path weighted according to slope
		# (a) choose direction with largest drop (or shallowest uphill...)      
		if(random)
		{      
			p <- abs(clens*slopes/sum(abs(clens*slopes)))    				
			flowdest <- WeightedRandomSelection(p)
			
		}
		else
		{
			# always choose steepest slope (equivalent to single-direction flow algorithm)
			flowdest <- which(slopes==min(slopes))[1]  
		}
		nextcell <-  downslope[flowdest]
		return(nextcell)		
	}
	# 
	return(NULL)
}
