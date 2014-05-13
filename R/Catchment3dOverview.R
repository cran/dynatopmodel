Catchment3dOverview <-
function(dem, 
                                drn=NULL, 
                                dem.cols=NULL,    # dem supplying draping colours
                                
                                phi=60, theta=0, border=NA, box=F,
                                add.ctrs=T,
                                nlevels=10, ctrs.lty=2, ctrs.lwd=1, ctrs.col="black",
                                drn.lty=1, drn.lwd=2, drn.col="navy",
                                drn.dash=NULL,  # can supply precalculated transformed drn to save time
                                ...)
{
  if(is.null(dem)){return(NULL)}
  if(!is.null(dem.cols))
  {
    # use supplied raster as basis for colouring faets in perspective plot
    pmat<- drape.plot.raster(dem, dem.cols, box=box, phi=phi, theta=theta, border=border, d=5,...)    
  }
  else
  {
    pmat<-persp(dem, box=F, phi=phi, theta=theta, border=border, ...)
  }
  if(add.ctrs)
  {
    # convert dem cvontour lines to spatial lines
    ctrs <- rasterToContour(dem, nlevels=nlevels)
    # debugonce(Get3dTransformedDRN)
    ctrs.dash <- Get3dTransformedDRN(dem, ctrs, pmat)
    
    lapply(ctrs.dash, 
           function(coords)
           {
             lines(coords$x, coords$y, col=ctrs.col,lwd=ctrs.lwd, lty=ctrs.lty)
           })
  }
  
  if(!is.null(drn))
  {
    cat("Transforming river network...")
    # river network overlain on dem to 2d affine projection using projection matrix determined above
    
    drn.dash <- Get3dTransformedDRN(dem, drn, pmat)
  }
  if(!is.null(drn.dash))
  {
    
    lapply(drn.dash, 
           function(coords)
           {
             lines(coords$x, coords$y, col=drn.col,lwd=drn.lwd, lty=drn.lty)
           })
    
  }
  return(list("pmat"=pmat, "drn.dash"=drn.dash)) #ctrs.dash"=ctrs.dash))
}
