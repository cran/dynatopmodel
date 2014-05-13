DEMFromRaster <-
function(rast,res=NULL,title=NULL)
{
	# if input already a matrix then return (no info available about res in this case so assume this is known elsewhere)
	if(is.matrix(rast))
	{
		res <- res
		title <- title
		dem <- rast
		# BL is assummed to be 0,0 but we can calculate the extent from the matrix dims and grid size
		extent <- extent(0, (ncol(rast)-1)*res,
						 0, (nrow(rast)-1)*res)
	}
	else
	{
		# otherwise need to convert the raster data to matrix and flip and transpose to match
		# the R convention for delaing with matrix "rasters"
		
		title <- rast@title
		# assuming here that the x and y resolutions are equal
		res <- xres(rast)
		# needs some manipulation to get the data in the right order 
		# i.e. raster treats columns as y, rows as x and proceeds from top to bottom i.e first value
		# is top left of map (NW), the complete opposite to the standard R routines for displaying
		# elevation grids
		# use either rast values or 
		dem<- matrix(nrow=rast@ncols, ncol=rast@nrows, getValues(rast))
		extent <- extent(rast)
		# invert vertically
		dem<-dem[,ncol(dem):1]
	}

	return(list("title"=title,"res"=res,"extent"=extent,"dem"=dem))
}
