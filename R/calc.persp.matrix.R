calc.persp.matrix <-
function(run.par, disp.par)
{
  dem <- run.par$dem
  if(is.null(dem)){
    stop("DEM raster required for spatial output")
  }
  # these could also be supplied from input
  disp.par$x <- xres(dem)*0:(nrow(dem))
  disp.par$y <- yres(dem)*0:(ncol(dem))   

  dem <- as.matrix(dem)
  # elevs are midpoints of cells - produce a matrix with averaged elevs
  # at edges of cells
  z <- dem
  z1 <-rbind(z[1,],z)
  z1 <-cbind(z1[,1],z1)  
  z2 <-dem
  z2 <-rbind(z2, z2[nrow(z2),])
  z2 <-cbind(z2, z2[,ncol(z2)]) 

  disp.par$z <-(z1+z2)/2    # average
  
  cm <- round(run.par$cm)
  # map class names to order
  # substitute the saturation percents for the hsu ids  
  # subst.tab <- cbind(groups, "psat"=pc) 
  # doesn't really matter here as long as groups stay in thre right order
  class.ids <- unique(cm, na.rm=T)
  
  disp.par$cm.map <- as.matrix(subs(cm, data.frame(class.ids, 1:length(class.ids))))
  
  # plot the dem once to get the transformation matrix
  with(disp.par,  
    pmat<-persp(x=x, y=y,shade=0.2, 
              z=z, , 
              theta=graphics.spatial.theta, 
              expand= graphics.spatial.expand, phi=graphics.spatial.phi,
              box=F,    
              border="gray")
  )
  
  if(!is.null(run.par$drn))
  {
    # transform the river channel network to 2d for overlay on plot
  #  message("projecting river network for display...")
   # disp.par$drn.dash < Get3dTransformedDRN(run.par$dem, run.par$drn, pmat)
  }
  
  return(disp.par)
}
