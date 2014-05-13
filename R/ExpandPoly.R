ExpandPoly <-
function(drn)
{
  if(is.list(drn)){return(drn)}
  crs <- CRS(projection(drn))
  drnexp<-lapply(drn@polygons[[1]]@Polygons, 
                 function(l)
                 {
                   
                   p<-Polygons(list(l), ID="1")
                   sp <- SpatialPolygons(list(p), proj4string=crs)
                   
                 })
  
  
  for(l in 1:length(drnexp))
  {
    drnexp[[l]]@polygons[[1]]@ID <- as.character(l)
  }
  return(drnexp)
}
