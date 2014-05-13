build.hru.table <-
function(cm,    
                       dem=NULL, 
                       reaches=NULL,
                       cellareas=cm-cm+1,  # props occupied by land 0-1
                       catch=NULL)  
{
	if(is.null(catch))
	{
		catch=cm[[2:nlayers(cm)]]
		# assume that catchment discretistaion info is passed via the other layer of
		# the multi-band raster
		cm <- cm[[1]]		
	}
	
	ids <- unique(cm[[1]])
 	# handle outside the class matrix 
  cellareas[which(is.na(cellareas[])&!is.na(cm[]))]<-1
	# maximum plan area of land within each cell
  maxCellArea <- xres(cm)*yres(cm)
	
	cat("Building areas...")  
	areas<- sapply(ids, 
                 function(id)
                   {                                        
                    cm.props <- cellareas[]*(cm==id)[]
             #       browser()
                    sum(cm.props[], na.rm=T)* maxCellArea
                 }
	)
  
	# add in river reaches, removing the area covered by channel from the
	# corresponding groups

	if(!is.null(reaches))
	{
 #   reaches <- reset.origin(reaches)
#    cellareas <- reset.origin(cellareas)
	  #cellareas <- crop(cellareas, extent(reaches))
		# determine land areas occupied by each channel and insert ids and areas
		# at head of group table
    
		chans <- zonal((1-cellareas)*maxCellArea, reaches, fun=sum) # sum the areas of cells classed by channel id multiplied by proportion ocuupied for edach
		# two colums: id, area (nas removed)
		areas <- c(chans[,2], areas)
		ids <- c(chans[,1], ids)
   # browser()
# 		# get unique nin-zero reach ids
# 	  rids <- setdiff(unique(reaches[[1]]), 0)
# 	  nreach <- length(rids)
# 	  
# 	  lapply(rids,
# 	  			 
# 	  			 function(id)
# 	  			 {
# 	  			 		maxCellArea*sum(((reaches[[1]]==id)*(1-cellareas))[], na.rm=T)
# 	  			 }
# 	  )
#     for(rid in rids)
#   	{
#   	  rarea <- maxCellArea*sum(((reaches[[1]]==rid)*(1-cellareas))[], na.rm=T)
#   	  areas <- c(rarea , areas)
# #  	  ids <- c(paste0("r",rid), ids) 	  
#   	}  
# 	  rids <- paste0("r",rids)
# 		ids <- c(rids, ids)	  
	}

  # total catchment area (includes reaches)
  totArea <- sum(areas)

	# reordering so that river reaches appear first. add 100 to distinguish reaches from land hsus
#	orders <- c(100+((nchan+1):ngroups), 1:nchan)	

	# build table, first two columns will be used to renumber the 	
	groups <- data.frame("id"=ids,
	                     "tag"=ids,
                       "chan.no"=NA,
						 "order"=1:length(ids),
		#				 "breaks"=ints,
						 "area_pc"=round(100*(areas/totArea),2),
						 "area"=round(areas))
	groups <- cbind(groups, "atb.bar"=0) 

  groups <- Addupslope.areas(groups, dem, cm, 
  													area_pcs=cellareas)
	groups$atb.bar <- round(groups$atb.bar, 2)
	groups <- cbind(groups, "gauge.id"=1)     
  groups <- cbind(groups, "catch.id"=1)
  nms <- names(def.hsu.par())
  pars <- data.frame(matrix(rep(def.hsu.par(), nrow(groups)), byrow=T, nrow=nrow(groups)))
  colnames(pars) <- nms
  groups<- cbind(groups, pars)
  groups <- apply(groups, MARGIN=2, FUN=function(x){unlist(x)})

	row.names(groups) <- groups[,1]
	return(groups)	
}
