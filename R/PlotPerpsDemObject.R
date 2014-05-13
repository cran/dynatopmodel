PlotPerpsDemObject <-
function(dem, obj, type="l", pmat=NULL, lcol="black", lwd=1, lty=1, ...)
{
  # if pmat supplied use to transform object coords. otherwise take from
  # result of perspective plot of dem
  if(is.null(pmat))
  {
    pmat <- persp(dem,...)
  }
  xy <- GetCoords(obj)
  z <- extract(dem, xy) #[[1]]
  # projected coordinates 
  xyp<-trans3d(xy[,1],xy[,2],z,pmat)

  xyp <- cbind(xyp$x, xyp$y)
  
  if(type=="l")
  {
    lines(xyp, lwd=lwd, col=lcol,  lty=lty,...)
  }
  return(pmat)
}
