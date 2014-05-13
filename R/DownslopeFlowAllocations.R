DownslopeFlowAllocations <-
function(rast, cur, 
									 thresh=0)  # thresh is maximum slope to allow downslope flow
	# +ve value allows flow to go "uphill"
{
  # only consider raster cell that have a classification attached
  rast2 <- raster::setValues(rast, NA)
  rast2[cur]<-rast[cur]
  # ensure there are flow directons for all the elevation cells considered
  rast <- fill.sinks(rast, deg=0.1, silent=F)
	# adjacent cells
 # cur <- cur[which(!is.na(rast[]))]
	adj <- adjacent(rast,cur,directions=8, pairs=T)	
	# select only directions with -ve (downslope) flow
	dz <- rast[adj[,2]] - rast[adj[,1]]
	good <- which(!is.na(dz) & dz <= thresh)
  adj <- cbind(adj[good, ], dz[good], NA) 
  
  # divvie up flow direction by cell 
  cells <- split(adj, as.factor(adj[,1]))
  ncells <- length(cells)
#	pb <- txtProgressBar(max=length(ncells), title="flow allocations", style=3)
#	on.exit(close(pb))
  adj <- lapply(cells,
    function(cell.dirs)
    {
      # row index
   #   setTxtProgressBar(pb, which(adj[,1]==names(cell.dirs))/ncells)
      # rebuild the destination cell matrix
      adj <- matrix(cell.dirs, ncol=4)
      # sum 
      dz.cell <- adj[,3]  #adj[i.adj.cell,3]
      if(all(dz.cell==0))
      {
        #browser()
        # deal with flat areas by allocating equally in all directions
        p <- 1/length(dz.cell)
      }
      else
      {      
        # calculate weighted averages in each direction out of 
        p <- abs(dz.cell/sum(dz.cell))
      }
      adj[,4]<-p
      return(adj)	
  	}
  )
 # co-erce back to a table and remove any nulls
 adj <- do.call(rbind, adj)
  # table: first col is source, second destination, third proportion of flow in that direction
	return(adj[,c(1:2,4)])
	# no downslope cells
	return(NULL)
}
