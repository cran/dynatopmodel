HighlightCell3d <-
function(dem,    # the dem that 
                            cell=NULL,   # cell number or x y coords
                            xy = NULL,
                            pmat,    # affine transformation matrix obtained from call to perps or fields::drape.plot
                            label=NULL,
                            ...)  # col gives fill colour, border the edge colour
{
  if(is.vector(xy))
  {
    # should be a vector of two elements
    cell <- cellFromXY(dem, xy)
    
  }
  ext <- CellExtent(dem,cell)	
  z <- dem[cell]
  xy.dash<-trans3d(c(ext@xmin, ext@xmax), c(ext@ymin, ext@ymax), rep(z, 2), pmat=pmat)
  x.dash <- xy.dash$x
  y.dash <- xy.dash$y
  
  rect(xleft=x.dash[1], xright=x.dash[2], 
       ybottom=y.dash[1], ytop=y.dash[2], ...)
  #	}
  # label with cell number, centred
  if(!is.null(label))
  {
    xbar <- sum(x.dash)/2
    ybar <- sum(y.dash)/2
    text(labels=label, x=xbar, y=ybar,...)
  }
}
