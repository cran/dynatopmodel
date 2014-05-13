build.hru.spatial <-
function(disc, drn=disc$drn)
{
  if(!is.null(drn))
  {
    # convert drn to polygons
    drn <- rgeos::gBuffer(drn, width=max(round(disc$chan.width/2), 1), byid=T)
  }
#  drn <- SpatialPolygonsDataFrame(drn, data=data.frame())
  # create a spatial polygons dataframe object from the given 
  # for the given discretisation  
  ids <-  disc$groups[,"id"]	
  ngroups <- length(ids)  # includes river
  
  hru.rast <- disc$hru.rast
  
  cat("Building HRU polygons...")
  hru.sp <- rasterToPolygons(disc, dissolve=T)
  polys <- hru.sp@polygons
  poly.ids <- unique(hru.rast)
    #     if(!is.null(drn))
    #     {
    #       drnbuff <- gBuffer(drn, width=max(round(chan.width/2+0.5), 1), byid=T)
    #       polys <- c(drnbuff@polygons, polys)
    #       poly.ids <- c(1:length(drnbuff@polygons), poly.ids)
    #     }  
    # reconstruct SpatialPolygons object
  hru <- SpatialPolygons(polys, proj4string=hru.rast@crs)
  # check that group and polygon ids are equal
  try(if(!all(ids==poly.ids)){warning("error in groups and hru class ids")})
  hru <- maptools::unionSpatialPolygons(hru, IDs=poly.ids)  
    
  dat <- data.frame(disc$groups[disc$groups[,"id"] %in% poly.ids,])
  row.names(dat) <- dat$id 
  hru <- SpatialPolygonsDataFrame(hru, dat)  
  
}
