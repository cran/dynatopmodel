determine.reaches <-
function(dem, drn, reaches=NULL, cellprops=NULL, chan.width=2)
{
  if(!is.null(reaches))
  {    
    reaches <- crop(reaches, dem)
    # multiband - second layer is proportion of cells occupied by channel
    #  reaches <- stack(fn)
    # if no proportions specified, use ratio of channel width to raster resolution
    if(nlayers(reaches)>1){
      cellprops <- reaches[[2]]   
    }
    
  }   
  else if(!is.null(drn))
  {    
    # build the reach multi band raster and save
    message("Building raster for channel(s)...")  	
    reaches <- build.reach.raster(dem, drn, 
                                chan.width=chan.width)     
    cellprops <- reaches[[2]]

  }
  else
  {     
    # use the TWI to idenifty the channel
    reaches<- upslope.area(dem)$atb>10
  }  
  
  if(is.null(cellprops))
  {
    
    # take an estimate of te area of each cell occupied by channel
    cellprops <- (reaches>0)*chan.width/xres(dem)
    # build stack as required
    reaches <- addLayer(reaches, cellprops)    
  }  
  # ensure only integer ids
  reaches[[1]]<- round(reaches[[1]])
  
  # if drn greater extent than then reaches raster can be of greater extent than dem
  try(reaches <- crop(reaches, extent(dem)))
    
  return(reaches)
}
