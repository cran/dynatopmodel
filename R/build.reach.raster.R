build.reach.raster <-
function(dem, drn, nchan=nrow(drn), 
                               chan.width=1)
{
  reaches <- rast.mem.copy(dem)
  reaches[] <- NA
  # one row per channel
  for(i.reach in 1:nrow(drn))
  {
    reach <- drn[i.reach,]
    
    #id <- reach@lines[[1]]@ID
    id <- i.reach # for time being enforce strict sequential ordering of reeach ids
    reach.cells <-  sapply(extract(dem, reach, cellnumbers=T), 
                         function(r)
                         {
                           r[,1]
                         })
    reaches[reach.cells]<-id
  }
  
  # add a layer with proportions of cell occuppied by channel. estimated by 
  # proportion of cell size to channel width, probably close enough
  reaches <- addLayer(reaches, (reaches>0)*chan.width/xres(dem))

  return(reaches)
}
