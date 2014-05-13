disp.spatial <-
function(groups,                       
                            vals,                                                         
                            cm,   # class matrix: dem not required - should be precomputed. Or could add to cm so only one disk access required
                           # disp.par contains the necessary info to show elevation perspective plot
                           # and parameters for plot display e.g. horz and vertical view angles and vert expansion factor                           
                           disp.par,  # 
                            range.vals=range(vals),  # max sd (mm) for areas that are shaded (> is white) 
                            time=NULL,  # simulation time                          
                            ichan=1,  # channel ids                          
                            nlevels=10,
                            ramp = colorRampPalette(c("white", "blue"), bias=1),
                            main="",  # "Storage deficits (mm)"                            
                            expand=disp.par$graphics.spatial.expand, 
                            theta=disp.par$graphics.spatial.theta, 
                            phi=disp.par$graphics.spatial.phi)
{  
  groups <- groups[-ichan,]
  # remove channels
  if(length(vals==length(ichan)+length(groups))){
    vals <- vals[-ichan]  
  }
  if(is.null(cm)){
    stop("Classification raster required for spatial output")
  }
  # get colours 
  satRamp<- ramp(nlevels)
  min.val <- range.vals[1]
  max.val <- range.vals[2]
  range <- max.val - min.val
  # most interested in areas close to saturation (i.e top m) - display these in shades of blue
  # areas with larger deficits are white. any vals > max are set to max 
  vals <- pmin(vals, max.val)
  vals <- pmax(vals, min.val)  # -ve values not allowed
  props <- nlevels*(vals-min.val)/range
  pc <- round(props+1)
  
  cols <- satRamp[pc]

  # determine facet colours for persp plot. fields::fields::drape.colors handles midpoints
  drape.cols <- fields::drape.color(zlim=range(cm[], na.rm=T), cm,
                            midpoint=F, col=cols)$color.index
    
  border<-"gray"
     
  x <- disp.par$x
  y <- disp.par$y
  z <- disp.par$z
  if(length(z)>1e4)
  {
    # No cell  boundaries if large grid
    border <- NA
  }
   
  # pmat is affine transformation matrix to project 2d to 3d coordinates
  pmat<-persp(x=x, y=y,shade=0.1, 
              z=z, col=drape.cols, theta=theta, expand=expand, phi=phi,
              box=F,    
              border=border,
              #legend=F, 
              d=0.75,
              main=main, cex.main=1)
  
  fields::image.plot(legend.only=T, col=satRamp, legend.shrink=0.5, legend.mar=3,
          #   label.breaks = 0:(nlevs-1),
             zlim=c(0, max.val), horizontal=T)

}
