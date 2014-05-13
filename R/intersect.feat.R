intersect.feat <-
function(cb, dn, pattern="*.shp",
                           out.dn=NULL) #file.path(dn, "morland"))
{
  fns <- dir(dn, pattern)
  plot(cb)
  #  i<<-1
  #  pb<<-txtProgressBar(0, length(fns)
  shps <- lapply(fns, function(fn)
  {
    res <- NULL
    cat(fn, "...")    
    ln <- gsub(".shp", "", fn)
    shp <- rgdal::readOGR(dn, ln)
    shp <- sp::spTransform(shp, CRS(projection(cb)))
    try(res <- gIntersection(cb, shp))
    if(!is.null(res))
    {
      try(plot(res, add=T))
      if(!is.null(out.dn))
      {
        try(res <- get.inter.geom(res, shp))
        try(writeOGR(res, out.dn, ln, driver="ESRI Shapefile"))
      }
    return(ln=res)
    }
  })
  shps <- delete.NULLs(shps) 
  #
 
  return(shps)
  
}
