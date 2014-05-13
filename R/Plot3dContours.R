Plot3dContours <-
function(dem, nlevels=10, type="l", pmat=NULL, lcol="black", 
                           lwd=1, box=F, ...)
{
  df <- rasterToContour(dem,nlevels=nlevels)
  
  # df should be a data frame of objects
  # get the transformation matrix
  pmat<-PlotPerpsDemObject(dem, obj=df[1,], lwd=lwd, lcol=lcol, box=box, ...)

  for(i in 2:nrow(df))
  {
    PlotPerpsDemObject(dem,df[i,], pmat=pmat,lwd=lwd, lcol=lcol,...)

  }
  return(pmat)  
}
