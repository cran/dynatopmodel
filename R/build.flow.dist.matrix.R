build.flow.dist.matrix <-
function(dem, cm,  # landscape units
#                   raster built from spatial object representing channel. If supplied, it should be a spatialines
#                   data frame with a row for each  reach. In the weighting matrix HSU ids are
#                   created for each reach and inserted before the landscape
#                   HSUs. Landscape cells that contain part of the channel flow to that reach,whatever the local topography
                  drn=NULL,
                  reaches=NULL,
									ndp=3,
								  all=F)
{ 
#  dem <- rast.mem.copy(dem) 
#  reaches <- rast.mem.copy(reaches) 
#  cm <- rast.mem.copy(cm) 
	# check	that rasters have same dims and resolution
#	compareRaster(dem, cm, res = TRUE)
	# note: these are the ids that indicate HSU classifications, which may contain 
	# information on how the hsu was discretised. The transition matrix
	# assumes that they are in sequential order, which can be obtaied from the sort order
	# as teh discretisation retains the structure of the groupings in the order
	# save original ids
	if(all)
	{
		# include every cell, including those outside catchment
		hsuids <- 1:length(cm)
		cellnos <- hsuids
	}
	else
	{
		# just non-NA 
		hsuids <- unique(cm[[1]], na.rm=T)  # 1:max(cm[], na.rm=T)}	
		cellnos <- which(!is.na(cm[]))		
	}

	#reaches <- determine.reaches(dem, drn, reaches, chan.width=4)
	reachids <- cm-cm
  cellprops <- cm-cm
  
  if(!is.null(reaches))
  {   
    reachids <- reaches[[1]]
    # proportion of cells occupied by reach
    cellprops <- reaches[[2]]	     
    # reorder so that reach ids are sequential
    rids <- unique(reaches[[1]])
    reachids <- subs(reaches[[1]],
                     data.frame(rids, order(rids))
      )
    # insert a hsu for every reach 
    hsuids <- c(unique(reachids), hsuids)   
  }
  
	# reclass raster so sequential ids are used
	cm <- raster::subs(cm, data.frame(hsuids, order(hsuids)))

	cat("Getting downslope flow weights...\n")
  start.tm <- Sys.time()
  down.all <- DownslopeFlowAllocations(dem, cellnos)
	cat(nrow(down.all), "directions processed in ", format(difftime(Sys.time(), start.tm), digits=2), "\n")
 # down.all <- down.all[which(!is.na(down.all[,2])),]
  
  if(length(down.all)==0)
  {
    warning("no flow paths identified")
    return(matrix(0, nrow=length(hsuids), ncol=length(hsuids)))
  }
  from <- down.all[,1]
  to <- down.all[,2]
	down.all <- cbind(down.all, cm[from], cm[to])

  # add in river transitions
	reach.cells <- which(reachids[]>0) 
  ireach <- which(down.all[,2] %in% reach.cells)
  rtrans <- down.all[ireach, ]
  # proportion of flow from these cells into channel (rather than identified HSU transition)
  props <- cellprops[rtrans[,2]]
  # build extra rows two for each cell- river cell transition
  # split flow according to proportion of cell occupied by channel
	riv.props <- props*rtrans[,3]   # third col is proportion
  land.props <- (1-props)*rtrans[,3]
  # replace with updated land transitions and append river transitions
  down.all[ireach,3] <- land.props
  # create new rows. 1 is from cell, 2 dest cell. Replace col 3, prop, with calculated value
  # from HSU teh same, last col is destination HSU id - river ids come before HSU ids
  new.rows <- cbind(down.all[ireach, 1:2], riv.props, down.all[ireach,4], reachids[rtrans[,2]])
  down.all <- rbind(down.all, new.rows)  
  # HSU and channel transistion table: from hsu, to hsu (or channel), flow proportion
  trans <- data.frame(as.factor(down.all[,4]), as.factor(down.all[,5]), down.all[,3])
  names(trans)<-c("from", "to", "prop")
  # cross tabulate
  cat("Cross tabulating inter-group transitions...\n")
  w <- xtabs(prop~from+to, data=trans)  
  if(any(rowSums(w)==0))
  {
    warning("Nil row sum in flow matrix - check dem")
  }
# w <- signif(w, ndp) 
	# round to sensible no. dp and renormalise to rows add to 1
  w <- normalise.rows(w)

	if(!is.null(reaches))
	{
  # insert an identity matrix for channel transistions, if any.
  # Can either leave as is
  # and route all channel flow via a time delay procedure and /or construct
  # inter channel flow transition matrix to route flows down channel
  nreach <- length(unique(reachids))
  rw <- matrix(0, ncol=ncol(w), nrow=nreach)
  rw[1:nreach, 1:nreach] <- identity.matrix(nreach)
  w<-rbind(rw, w)
	# normalise weights so that sum of flow out of each hsu is 1 and round to sensible 
  # figure
	}
	return(w)
}
