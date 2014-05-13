init.hru <-
function(groups, 
                            params=def.hsu.par(),
                            chan.par=def.chan.par(),
							              ichan=1, rain=NULL)
{
  # groups is table of area groupings and params is list of default parameter values
  # Search for columns for saturation transmissivity ln_t0, max root zone storage srMax 
  # and exponential transmissivity profile factor m 
  
  #groups <- AddParam(groups, "ln_t0", params$ln_t0)
  #groups <- AddParam(groups, "m", params$m)
  #groups <- AddParam(groups, "SRZMax", params$SRMax)
  
#   if(!"ln_t0" %in% names(groups))
#   {
#     groups <- cbind(groups, list("ln_t0"=params$ln_t0))
#   }
#   if(!"atb.bar" %in% names(groups))
#   {
#     # dummy areal average of topographic index?
#     groups <- cbind(groups, list("atb.bar"=params$ln_t0))
#   }  
  # add missing params using defaults. groups' parameters override defaults
	nms <- setdiff(names(params), colnames(groups))
	if(length(nms)>0)
	{
		dat <- matrix(rep(params[nms], nrow(groups)), nrow=nrow(groups),byrow=T)
		colnames(dat)<-nms
		groups <- cbind(groups, dat)
	}
#    for(igroup in 1:nrow(groups))
#    {
#      pars <- as.list(groups[igroup,])
#      groups[igroup,] <- as.vector(merge.lists(pars, def.hsu.par()))      
#    }
	if(length(ichan)>0)
	{
	  LogEvent(paste0("Overriding parameters for ", length(ichan), "HSU group(s) identified with river reaches\n"))
	  # the "river" has zero root zone storage and there is no time delay in drainage reaching baseflow 
	  # Storage is limited only by bankfull level - set SD and SDMax to large values to simulate 
	  # Could apply physically realistic max storage (i.e average river depth at bankfull level) to simulate overbank events
	  groups[ichan,]$srz0 <- chan.par$srz0 
	  groups[ichan,]$srz_max <- chan.par$srz_max   # rainfall goes straight to the unsat zone infiltration
	  groups[ichan,]$sd_max <- chan.par$sd_max   # maximum allowable SD
	  groups[ichan,]$vof <- chan.par$vchan   # routing velocity
	  
	#  groups[1:nchan,]$SD <- 3  # this is a huge value reflecting the channel's storage capacity
	  groups[ichan,]$td <- chan.par$td  # all drainage will be routed directly to base flow
	}
	# checj taht groups wetness is in decreasing order
	#if(!all(order(groups[-ichan,$atb.bar, decreasing=T)==groups[-ichan,]$order)){warning("Wetness index not in decreasing order of HRU number")}
  # labels for groups default to ids (shoudl these be sequential?)
  if(is.null(groups$tag)){groups$tag<-groups$id}
	# max base flow when deficit =0
	try(groups$qbmax <- exp(groups$ln_t0-groups$atb.bar), silent=T)
	n.gauge <- ifelse(is.null(rain), 
										1, 
										ncol(rain))
  # ensure gauge reading supplied is withing range
  groups$gauge.id <- pmin(groups$gauge.id, n.gauge)

  return(groups)  
}
