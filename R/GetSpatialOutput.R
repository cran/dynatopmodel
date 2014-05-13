GetSpatialOutput <-
function(groups, flows, stores, disp.par, maxq)
{
  if(disp.par$graphics.spatial.output=="qbf")
  {
    # expand to mm/hr
    vals <- 1000*flows$qbf
    range.vals <-  c(0, maxq)
    ramp <- colorRampPalette(c("white", "blue"))  
    main <- "Specific base flows (mm/hr)"
    nlevels<-10
  }
  else if(disp.par$graphics.spatial.output=="surface.storage")
  {
    #  if(any(stores$ex>0)){browser()}
    # overland flow
    vals <- 1000*stores$ex#/groups$sd_max
    ramp <- colorRampPalette(c("white", "blue"))        
    range.vals <-  c(0, 2)
    nlevels<-10
    main <- "Overland storage (mm)"        
  }
  else
  {     
    vals.sd <- 1000*stores$sd#/groups$sd_max
    # reversed colour map: close to saturation indicated by blue
    ramp.sd <- colorRampPalette(c("blue", "wheat", "white"))        
    range.vals.sd <-  c(0, 150)
    
    #   vals <- vals.qbf
    vals <- vals.sd
    range.vals <- range.vals.sd
    ramp <- ramp.sd
    nlevels<-20
    main <- "Storage deficits (mm)"
  }
  return(list("vals"=vals, "main"=main, "nlevels"=nlevels, "ramp"=ramp, "range.vals"=range.vals))
}
