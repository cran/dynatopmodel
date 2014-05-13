Get3dTransformedDRN <-
function(dem, drn, pmat)
{
  coord.list <- GetCoords(drn)
  #coord.list<- unlist(coord.list, recursive=F)
  
  if(is.list(coord.list))
  {coord.list<-unlist(coord.list, recursive=F)}
  
  trans<-lapply(coord.list,
                function(crds)
                {
                  
                  # if(is.list(coords)){coords<-coords[[1]]}
                  # browser()
                  x <- crds[,1]
                  y <- crds[,2]
                  z <- extract(dem, crds)
                  crds.dash <- trans3d(x,y,z,pmat)
                })
  
}
