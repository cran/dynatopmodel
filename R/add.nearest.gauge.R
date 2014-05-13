add.nearest.gauge <-
function(groups, hru.sp, drn, gauges)
{
  ids <- groups$id 
#  groups <- sort(groups$hru.sp)
  
  for(i.group in 1:nrow(groups))
  {
    id <- groups[i.group,]$id
    hru.geom 
    # locate the geometry associated with the group
    hru.geom <- hru.sp[which(hru.sp$HRU==id),]
    cent <- rgeos::gCentroid(hru.geom)
    # locate current max
    dist <- rgeos::gDistance(cent, gauges[i.gauge,])    
    for(i.gauge in 1:nrow(gauges))
    {
      if(rgeos::gDistance(cent, gauges[i.gauge,])<dist)
      {
        groups[i.group,]$gauge.id  <- i.gauge 
      }      
    }
  }
  hru.sp@data <- groups
  return(hru.sp)
}
