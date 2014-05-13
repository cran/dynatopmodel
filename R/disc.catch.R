disc.catch <-
function(dem,                                               
                       catch,
                       cuts=NULL,  # definition of how the catchment is to be discretised                       
                       drn=NULL,
                       reaches=NULL, # reaches raster is calculated from DRN or supplied
                       cellprops=NULL,  # proportion of river cells occuppied by channel
                       chan.width=NULL,  # width of cahnnel in m, or equivalent DEM units.
                       area.thresh=NULL,
											 groups=NULL,
											 routing=NULL,
                       w=NULL,
                       cm=NULL,
											 build.polys=F, 
											 ...)   # can instead supply cuts as additional parameters e.g atb=10
{
	# build names from extra parameters
	cuts <- merge.lists(cuts, list(...))

  if(is.null(area.thresh)){
    area.thresh<- get.defs()$area.thresh
  }
  
  if(is.null(chan.width)){
    chan.width<- get.defs()$chan.width
  }
  
  if(area.thresh>=1){area.thresh<-area.thresh/100}

  reaches <- determine.reaches(dem, drn, reaches, chan.width=chan.width)
  cellprops <- reaches[[2]]
  nchan <- length(unique(reaches[[1]], na.rm=T))
  #  nchan <- nrow(drn)
  # channel identifiers
  ichan <- 1:nchan  
  
  if(is.null(groups))
  {  
    if(is.null(catch))
    {
      message("Building upslope areas...")
      catch  <- upslope.area(dem, atb=T)     
    }
    else if (!("a" %in% names(catch)))
    {
      message("Building upslope areas...")
  		#  always use topographic indexes such upslope area and / or altitude, slope, aspect etc
      catch  <- addLayer(catch, upslope.area(dem, atb=T))      		
  	}
  	
  	message("Combining layers...")
  	cm <- combine.groupings(dem, catch=catch, 
    										 cuts=cuts, thresh=area.thresh)
    nms <- names(cm)
	  # reorder by upslope area
	  a_bar <- raster::zonal(catch[["a"]], cm[[1]])  
  	# second column is mean value
	  ords <- order(a_bar[,2], decreasing=T)
	  sub.df <- data.frame(cbind(a_bar[,"zone"], a_bar[ords,"zone"]))
	  cm[[1]] <- subs(cm[[1]], sub.df)
    names(cm)<-nms

  	message("Building group info table....")
    
	  # layers that went into discretisation are held in further layers of classification matrix: 
	  # pass into proc to produce group summary table
	  groups <- data.frame(build.hru.table(cm, dem=dem, reaches=reaches[[1]], 
	                          cellareas=1-cellprops))		
	
	  groups[ichan,"chan.no"] <- ichan
	  groups[ichan,"vof"] <- NA
	  groups[-ichan,"vchan"] <- NA
	}
	nms <- names(cm)  
	# invalid cuts removed
	layer.nms <- nms[2:length(nms)]  
#   gr.not.chan <- groups[-ichan,]
#   # reorder groups according to upslope area, or a close proxy, the mean atb
#   groups[-ichan,] <- gr.not.chan[order(gr.not.chan[,"atb.bar"], decreasing=T),]
# #  cat("Total of ", nrow(groups), " areal groups exceeding", 100*area.thresh, "%\n")
#   rownames(groups)<-groups$id
#   groups$order<-1:nrow(groups)

#  print(groups)
 
  build.w <- is.null(w)

  if(!build.w)
  {
    if(nrow(w) != nrow(groups))
    {
      warning(paste0("groups table has ", nrow(groups), " groups definitions, flow matrix has ", nrow(w), "rows"))
      message("Flow distribution matrix inconsistent with discretisation - rebuilding... ")   
      build.w <- T
    }
  }
	if(build.w)
	{
		message("Creating flow transistion matrix....")
		w <- build.flow.dist.matrix(cm=cm[[1]], dem=dem, reaches=reaches)
	 # always require some kind of channel identification raster even if constructed from DEM
	 message("Creating channel routing matrix....")
	 # construct flow connectivity graph using the reach identification raster previously loaded
	 # or built, using reach ids as the "classification" and flow weightings from dem.
	 adj <- build.flow.dist.matrix(cm=reaches[[1]], dem=dem, reaches=NULL)
	 
   # clear the weights for all river hsus, then add the adjacency matrix	   
   # overwrite channel entries of weighting matrix with river connections	
	 w<- as.matrix(w)
	 w[ichan,]<-0
	 
	 w[ichan, ichan] <- adj   
	}
#   # this allows us to manually set weights for individual transitions
# #  w <- normalise.rows(w)
 	w <- signif(w, 3) 
#   w<- as.matrix(w)
#   print(w)
   ids <- groups[,"id"]
# 	#dimnames(w)<- list(ids, ids)
  w <- as.matrix(w)
  rownames(w)<- ids
	colnames(w)<- ids

	if(is.null(routing))
	{
		message("Building routing table...")
		try(routing <- make.routing.table(dem, cm[[1]], drn=NULL, reaches=reaches))
	}
  return(list(
  "groups"=groups, 
  "hru"=cm[[1]],   
  "ichan"=ichan,              
  "w"=w,
  "layer.names"=layer.nms,
  "routing"=routing,
  "reaches"=reaches))

}
