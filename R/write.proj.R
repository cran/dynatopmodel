write.proj <-
function(proj, 
                       dir = proj$dir,  # location for DEM, drn etc
                       disc.dir = file.path(dir, "disc"),
                       obs.dir = file.path(dir, "obs"),
											 save.disc=T,
                     #  copy.source = T,  # if true the original files creating the discretisation will be copied to dir
                       save.obs=F)    # obseravtions
{  
  if(!file.exists(dir)){dir.create(dir, recursive=T)}   
  proj$disc.dir <- disc.dir
  proj$obs.dir <- obs.dir
  proj$dir <- dir
  
  if(!file.exists(proj$disc.dir)){dir.create(proj$disc.dir, recursive=T)}   
  
  proj$sim.start <- as.character(proj$sim.start)
  proj$sim.end <- as.character(proj$sim.end)
  # save options can be later reloaded with source
  try(dump(list=c("disp.par", "run.par", "dt", "ntt", "qmax", "qt0", "sim.start", "sim.end"), envir=as.environment(proj), 
       file=file.path(proj$dir, "proj.par")))
  
  #  write.table(proj$disc$w, file.path(proj$disc$dir, "weights.dat"), quote=F, col.names=T, row.names=F, sep="\t")
  
  #  write.table(proj$disc$groups, file.path(proj$disc$dir, "groups.dat"), quote=F, row.names=F, sep="\t")
  if(save.obs)
  {
    write.zoo(proj$obs$qobs, file=file.path(proj$obs$dir, "qobs.dat"), index.name="time", sep="\t")
    write.zoo(proj$obs$rain, file=file.path(proj$obs$dir, "rain.dat"), index.name="time", sep="\t")
    write.zoo(proj$obs$pe, file=file.path(proj$obs$dir, "pe.dat"), index.name="time", sep="\t")    
  }
  if(save.disc)
  {
	  # all the discretisations
	  proj$disc<-lapply(proj$disc,
	         function(disc)
	         {
	            disc$dir <- disc.dir.name(disc$cuts, dn=proj$disc.dir)
	
	            write.disc(disc)
	            return(disc)
	         }
	  )
  }
#  if(copy.source)
#  {
    try(raster::writeRaster(proj$dem, file.path(dir, "dem.tif"), overwrite=T))
    # channel proportions should be saved with the discretisation as they depend on the channel width specified
    # locations are dicretisation independent
    try(raster::writeRaster(proj$reaches, file.path(dir, "reaches.tif"), overwrite=T))
    try(rgdal::writeOGR(proj$drn, dir, "drn", driver="ESRI Shapefile", overwrite=T))
    try(rgdal::writeOGR(proj$sites, dir, "sites", driver="ESRI Shapefile"), silent=T)
 # }
  # 
  return(proj)
}
