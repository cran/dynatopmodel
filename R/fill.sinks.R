fill.sinks <-
function(dem, deg=0.01, silent=T, fail.if.not.complete=F)
{
	output <- capture.output(res <- raster::setValues(dem, 
                                            topmodel::sinkfill(as.matrix(dem), res=xres(dem), deg=deg)))
	if(!silent){message(output)}
#	if(fail.if.not.complete){stop(output)}
	return(res)
	
}
