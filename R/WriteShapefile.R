WriteShapefile <-
function(x, dsn=getwd(), ln, overwrite=T)
{
#	dsn <- getwd()

	if(overwrite)
	{
		fns<-dir(dsn, paste0(ln, ".*", sep=""))		
		if(length(fns))
    {
      if(readline("warning - file exists - overwrite?")=="y")
  		{
  			file.remove(fns)			
  		}
		}
	}
  if(!is.data.frame(x))
  {
    x <- SpatialLinesDataFrame(x, data=data.frame("id"=1:length(x)))
  }
	writeOGR(x,dsn=dsn,layer=ln, driver="ESRI Shapefile")
}
