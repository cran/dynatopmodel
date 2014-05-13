extract.water.feat <-
function(cb, dn)
{
  patt<-"Water.*shp"
  feats <- intersect.feat(cb, dn, pattern=patt)
  feats <- lapply(feats, function(x){if(inherits(x, "SpatialPolygons") | inherits(x, "SpatialLines")){return(x)}})
  return(delete.NULLs(feats))
         

}
