update.storages <-
function(groups, flows, stores, dtt, ichan, tm)
{
	# check for max saturated flow for grouping, in which case set SD to zero and 
	# route excess as overland flow update storage deficits using the inflows and 
	# outflows (fluxes) - note that sd is a deficit so is increased by base flow 
	# out of groups and increased by flow from unsaturated zone and upslope areas 
	# inc drainage from unsat zone
	# net outflow from land zone - adds to sd.
	bal <- dtt*(flows$qbf - flows$uz -flows$qin/groups$area)
	
	# inflow / outflow for channel is handled by the time delay histogram so storage isn't relevant here
#	bal[ichan]<-0
	stores$sd <- stores$sd + bal
	
	noflow <- setdiff(which(stores$sd>=groups$sd_max), ichan)
	if(length(noflow)>0)
	{
		message("SD > SDmax in ") #, paste(groups[noflow,]$tag, collapse=","), tm))
		stores[noflow,]$sd<- groups[noflow,]$sd_max 
		flows[noflow,]$qbf <- 0  # flows[noflow,]$qbf - (stores[noflow,]$sd - groups[noflow,]$sd_max) / dtt
	}  
	
	# do not allow base flow to reduce storage  below zero. This should be routed overland
	saturated <- which(stores$sd<=0)  #    #setdiff(which(stores$sd<=0), ichan)
	if(length(saturated)>0)
	{ 
		
		#if(any(ichan %in% saturated))
		{
	#		browser()
		}
		#LogEvent(paste("Base flow saturation in zones ", paste(groups[saturated,]$tag, collapse=",")), tm=time)
		#  browser()
		# transfer negative deficit to excess store
		stores[saturated,]$ex <- stores[saturated,]$ex  -  stores[saturated,]$sd 
		#       
		#     # add to overland storage
		#     flows[saturated,]$ex <-  
		stores[saturated,]$sd<- 0 
	} 
	
	# check channels
	return(list("flows"=flows, "stores"=stores))
}
