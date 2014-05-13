combine.groupings <-
function(dem,
                             	layers=list(),				
														 	catch=NULL,
							 								cuts=c(a=5), 
              								thresh=2/100,   # threshold hsu area contrib
														 	equal.areas =F ) # if T then groups are strictly equal in plan area  
{	
	if(is.null(catch))
	{ # has to be at least one layer supplied
		layers <- delete.NULLs(layers)
  	catch<- raster::stack(layers)
	}
  # select names in stack matching discretisation
  nms <- intersect(names(catch), names(cuts))

  if(length(nms)==0){stop("No layers found in catchment raster stack matching names of cuts to be applied")}

  # default is just one HSU representin entire catchment
	cm <- dem-dem+101

	ints <- list()
	#if(!all(names(cuts) %in% names(catch)))	
	# apply hsu id for point by combining the cut value in each layer	
	for(nm in nms)
	{    
		for(id in raster::unique(cm))
		{
			idcells <- which(cm[]==id)
			n.cut <- cuts[[nm]]
			# cut cells already in this grouping according to the sublevel
			if(is.null(n.cut))
			{
				stop(paste(nm, " specified as cut variable but no corresponding layer found"))
			}
			if(n.cut>1)
			{							
				laycutvals <- cut(catch[[nm]][idcells], n.cut, labels=F)						
			}
			else{
        # one cuts only so return just when a nin-NA values existin 
				laycutvals <- as.numeric(catch[[nm]][idcells]>=0)
			}						
			# build new id from top level category plus sub level
			cm[idcells]<-10*cm[idcells] + laycutvals
		}	
	}
  # need this to distinguish channel and land HSUs
  if(max(cm[],na.rm=T)<100)
  {cm<-cm+100}
  
  # merge smaller groups
  # source list
  cm <- merge.groups(cm, thresh)
  cm <- merge.groups(cm, ids=rev(unique(cm)), thresh)

  subs2 <- data.frame(cbind(unique(cm, na.rm=T), 100+order(unique(cm, na.rm=T))))
  cm <- subs(cm, subs2)
	names(cm)<-"HRU"
	# add in the dem and each discrteised layer
	cm <- addLayer(cm, catch)
  
	return(cm)
}
