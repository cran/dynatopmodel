merge.rasts <-
function(dn)
{
	rast.fn <- dir(dn, "*.tif", recursive=T, full.names=T)
	
	merged <- NULL
	for(fn in rast.fn)
	{
		cat("merging ", fn, "...\n")
		dem <- raster(fn)
		if(is.null(merged)){merged <- dem}
		else
		{
			try(merged <- raster::merge(merged, dem))
		}
	}
	return(merged)
}
