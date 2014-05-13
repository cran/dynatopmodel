SnapExtent <-
function(dem, ext)
{
	
	yres <- yres(dem)
	# shift by remainder of difference of extents
	yshift <- (ext@ymin - extent(dem)@ymin) %% yres
	ymin <-  ext@ymin - yshift
	yshift <- (extent(dem)@ymax-ext@ymin) %% yres
	ymax <- ext@ymax - yshift
	xres <- xres(dem)
	xshift <- (ext@xmin - extent(dem)@xmin) %% xres
	xmin <-  ext@xmin - xshift
	xshift <- (extent(dem)@xmax-ext@xmax) %% xres
	xmax <- ext@xmax + xshift
	
	return(extent(xmin, xmax, ymin, ymax))
}
