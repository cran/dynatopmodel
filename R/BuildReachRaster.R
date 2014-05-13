BuildReachRaster <-
function(dem, drn, nchan=nrow(drn), 
                             chan.width=2)
{
  if(dem@crs != projection(drn))
  {
    osgb <- get.p4s("osgb")
   # reaches <- raster::setValues(dem, NA)
    # proportions of cells occuppied 
    #cellprops <- dem-dem
    # need to reproject both drn and dem to the same system to prevent error in rasterize  
    drn <- sp::spTransform(drn, CRS(osgb))
   # reproject if required - ensure consitent reprojection
   if(projection(dem)!= osgb){dem <- raster::projectRaster(dem, crs=osgb)}
  }
 reaches <- raster::setValues(dem, NA)
 width<-max(chan.width %/% 2, 1)
  while(length(which(!is.na(reaches[])))==0)
  {
 # create a 2d representation of channels. byid maintain parent ids
  drn.buff <- gBuffer(drn, width=width, byid=T)  # t if multiple reaches

  reaches <- rasterize(drn.buff, dem)
 width <- width+1
  }
  # proportion of cells occupied by channel
  cellprops <- rasterize(drn.buff, dem, getCover=T)/100
    
#   props <- extract(dem, drn.buff, weights=T)
#   
#   for(ichan in 1:nchan)
#   {
#     chan <- drn[ichan,] 
#     # polygon with desired width (small)
#     chanbuff <- gBuffer(chan, width=max(chan.width %/% 2, 1), capStyle="FLAT", byid=F)
#     # proportions occupied by channel
#     props <- extract(x=dem, y=chanbuff, weights=T, cellnumbers=T)[[1]]
#     # first col is cell number of cell containing channel
#     cells <- props[,1]
#     cellprops[cells]<-props[,3]
#     id <- ichan #as.numeric(row.names(drn)[ichan])
#     #  cells <- extract(dem, drn[ichan,],cellnumbers=T)[[1]][,1]		
#     # first layer has reach ID
#     reaches[cells]<- id	
#   }
  reaches <- addLayer(reaches, cellprops)

	# ensure return value is within extent of dem
	reaches <- crop(reaches, extent(dem))
  return(reaches)
}
