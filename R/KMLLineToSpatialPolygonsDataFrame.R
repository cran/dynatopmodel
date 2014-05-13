KMLLineToSpatialPolygonsDataFrame <-
function(dsn,layer,id=layer, proj4string=CRS(as.character(NA)))
{

  # CONSIDER just first shape encountered in kml 
  spLines <- rgdal::readOGR(dsn=dsn, layer=layer)
  
  coords <- coordinates(spLines[1,]@lines[[1]]@Lines[[1]])


  # add first point to end to close polygon
  coords <- rbind(coords, coords[1,])
  p <- Polygon(coords)
  ps <- Polygons(list(p),id)
  sp <- SpatialPolygons(list(ps),proj4string=proj4string)
  spdf <- SpatialPolygonsDataFrame(sp, data.frame("id"=layer), match.ID=F)
  row.names(spdf) <- spdf@polygons[[1]]@ID
  return(spdf)
}
