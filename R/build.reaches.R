build.reaches <-
function(dem, drn, chan.width, atb.thresh=10)
{
	if(!is.null(drn))
	{        
		# build the reach multi band raster and save
		message("Building raster for channel(s)...")  	
		reaches <- BuildReachRaster(dem, drn, 
																chan.width=chan.width)    
		chan.props <- reaches[[2]]
		rebuild <- T
	#	try(writeRaster(reaches, fn))
	}
	else
	{     
		# use the TWI to idenifty the channel
		reaches<- upslope.area(dem)$atb >= atb.thresh
		# take an estimate of te area of each cell occupied by channe
		chan.props <- (reaches>0)*chan.width/dem$xres
	}
	
}
