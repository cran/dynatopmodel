upslope.area <-
function(dem, log=T, atb=F, deg=0.1)
{
	
	if(xres(dem)!=yres(dem)) {stop("x and y resolutions differ")}	
  # any sinks still present may give strange results
#  sink(file="e:/junk/sink.txt")
#  on.exit(sink(NULL))
	capture.output(dem <- invisible(raster::setValues(dem, topmodel::sinkfill(as.matrix(dem), res=xres(dem), degree=deg))))
  
  topidx <- topmodel::topidx(as.matrix(dem), res=xres(dem))
	a <- raster::setValues(dem, topidx$area)
  if(log)
  {
    a <- log(a)
  }  
  if(atb)
  {
    atb <- raster::setValues(dem, topidx$atb)
    # add the topographic index ln(a/tanB)
    a <- addLayer(a, atb)
    names(a)<-c("a", "atb")
  }
  return(a)

  
}
