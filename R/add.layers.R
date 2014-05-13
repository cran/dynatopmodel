add.layers <-
function(proj, reload=F)
{
	nms <-  dir(proj$dir, "\\.shp")
	if(!reload)
	{
		nms <- setdiff(nms, paste0(names(proj), ".shp"))
	}
	#  try(proj$dem <- rast.mem.copy(raster(file.path(data.dir, "dem.tif"))))
	#try(proj$drn <- rgdal::readOGR(data.dir, "drn"))
	# iterate through any shape files located and add verbatim
	for(shp in nms)
	{
		# name without extention
		shp.nm <- sub("*.shp", "", shp)
		cat("Loading shape file ", shp.nm, "...")
		try(proj[[shp.nm]] <- rgdal::readOGR(proj$dir, shp.nm))
		if(!is.null(proj[[shp.nm]])){cat("...done\n")}
		else{cat("...failed\n")}
	}
	return(proj)
}
