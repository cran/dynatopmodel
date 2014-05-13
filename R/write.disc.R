write.disc <-
function(disc, dn=disc$dir)
{
  if(!is.null(dn) & !file.exists(dn)){dir.create(dn, recursive=T)}   
  with(disc, dump(c("cuts", "area.thresh", "chan.width", "layer.names"), file=file.path(dn, "disc.par")))
  
  #try(writeRaster(disc$reaches, file.path(dn, "reaches.tif"), overwrite=T), silent=T)
  # row names contain distance infor
  try(write.table(disc$routing, file.path(dn, "routing.dat"), row.names=F, col.names=F, quote=F, sep="\t")) 
  try(writeRaster(disc$hru, file.path(dn, "hru.tif")), silent=T)
  try(writeRaster(disc$hru, file.path(dn, "hru.tif"), overwrite=T))
  try(write.table(disc$groups, file.path(dn, "groups.dat"), row.names=F, quote=F, sep="\t")) 
 # try(write.table(disc$reachinfo, file.path(dn, "routing.dat"),row.names=F, quote=F, sep="\t"))
  try(write.table(disc$w, file.path(dn, "weights.dat"),row.names=F, quote=F, sep="\t"))    
#  try(writeOGR(disc$hru, dn, "hru", driver="ESRI Shapefile", overwrite=T))  
#  try(writeRaster(disc$chan.props, file.path(dn, "chanprops.tif")))  
	try(writeRaster(disc$reaches, file.path(dn, "reaches.tif")), silent=F)
  try(write.table(disc$topidx.frame, file.path(dn, "topidx.dat"), row.names=F, quote=F, sep="\t")) 
	return(disc)
}
