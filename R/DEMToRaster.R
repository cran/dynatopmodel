DEMToRaster <-
function(dem, title="", dx=1, xmin=0, ymin=0, proj4str="+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.9996012717 +x_0=400000 +y_0=-100000 +ellps=airy +datum=OSGB36 +units=m +no_defs")
{
  # determine max x and y from the mins and the resolution (assumed equal in both directions)
  ymax = ymin+ncol(dem)*dx 
  xmax = xmin+nrow(dem)*dx 
	# invert vertically and transpose: raster displays x / y and row / col in opposite sense
	dem<-t(dem[,ncol(dem):1])
  # create and return the raster

  ras <- raster(dem, xmx=xmax, ymx=ymax, xmn=xmin, ymn=ymin,crs=proj4str)
  ras@title=title
	return(ras)
}
