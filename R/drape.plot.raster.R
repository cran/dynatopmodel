drape.plot.raster <-
function(z,    # e.g. dem with z coords
                              z2=z, # matrix supplying draping colours - dims and resolution must match z
                              cols=rev(terrain.colors(25)),                  
                              box=F, shade=0.5,...)
{
  x <- seq(extent(z)@xmin, extent(z)@xmax, length.out=ncol(z))
  y <- seq(extent(z)@ymin, extent(z)@ymax, length.out=nrow(z))
  z <- as.matrix(z)
  # flip and transpose matrix vals to get in format expected by persp plot
  z <- t(z[nrow(z):1,])
  # matrix supplying colours
  z2 <- t(as.matrix(z2)[nrow(z2):1,])
  
   drape.cols <- fields::drape.color(z2, col=cols, midpoint=F)$color.index
 # persp(x=x, y=y, z=z, col=drape.cols, box=box, shade=shade,...)
  
  fields::drape.plot(x=x, y=y, z=z, z2=z2, col=cols, #drape.cols,
             box=box, shade=shade,...)
  
}
