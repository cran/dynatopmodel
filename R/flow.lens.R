flow.lens <-
function(dem, 
											agg=1, # initial aggregation factor
											max.agg=4,
											outlet=NULL)
{
	lens <- raster::setValues(dem, NA)
	dem.agg <- dem
	while(agg <= max.agg & max(c(0,lens[]), na.rm=T)==0)
	{
		if(agg>1)
		{
			message("Trying a aggregated dem to determine flow lengths")
			# try a coarser 
			dem.agg <- raster::aggregate(dem, agg)
			#	reaches <- aggregate(reaches, )
		}
		dem.agg <- fill.sinks(dem.agg, deg=0.1)
		lens <- raster::setValues(dem.agg, flowlength(as.matrix(dem.agg)))
		agg <- agg+1
	}	
	agg <- agg-1
	# disaggregate
	if(agg>1)
	{
		message("Disaggregating back to original resolution...")
		lens <- raster::disaggregate(lens, agg, method="bilinear")
	}
	return(raster::xres(dem)*lens)
}
