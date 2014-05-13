ProjectSpatial <-
function(what, to)
{
	if(to=="osgb"){ 
		proj <- "+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.9996012717 +x_0=400000 +y_0=-100000 +ellps=airy +towgs84=446.448,-125.157,542.06,0.15,0.247,0.842,-20.489 +units=m +no_defs"
		
	} else if(to=="merc"){
		proj <- " +proj=get.p4s(merc) +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null +no_defs"
	}
	else
	{
		stop("Invalid projection specified")
	}
	
	if(class(what)[[1]]=="RasterLayer")
	{
		return(raster::projectRaster(what, crs=proj))
		
	}
	else  #(class(what)[[1]]=="sp")
	{
		return(sp::spTransform(what, CRS(proj)))
	}

	stop("No valid spatial object supplied to ProjectSpatial function")
}
