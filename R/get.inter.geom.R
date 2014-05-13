get.inter.geom <-
function(res, shp)
{
    dat <- over(res, shp, returnList=T)
    dat <- do.call(rbind, lapply(dat, function(x){x}))
    if(nrow(dat)==length(res))
    {
      if(inherits(res, "SpatialPoints"))
      {
        res <- SpatialPointsDataFrame(res, data=dat)
      }
      else if(inherits(res, "SpatialPolygons"))
      {
        res <- SpatialPolygonsDataFrame(res, data=dat)
      }
      else if(inherits(res, "SpatialLines"))
      {
        res <- SpatialLinesDataFrame(res, data=dat)
      }
    }
    return(res)
}
