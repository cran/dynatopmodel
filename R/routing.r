# ################################################################################
# routing of subsurface,  overland flow to channel and channel flows to the
# catchment outlet
###############################################################################


#-------------------------------------------------------------------------------
# input
# groups      : average effective overland
# w                 : flux distribution matrix
# ichan             : channel identifiers
# vof               ; overall overland flow velocity
#-------------------------------------------------------------------------------

get.routing <- function(groups,
                        w,
												routing,   # channel routing table, first column is average distance to outlet, second the proportion of flow entering at this upstream distance
                        ichan,      # channel identifiers
                        dt       # time step (hr)
                       )
{
	if(length(routing) == 0)
	{
		# everthing moved to outlet in one time step
		return(matrix(1))
	}
	# apply average channel wave velocity??? need a reach" table
	vchan <- max(groups[ichan,]$vchan, na.rm=TRUE)
  chan.dists <- routing[,1]
  groups <- groups[-ichan,]

  # time delay for flow once in channel(s) until reaching outlet
  # assumming a constant channel velocity
  dtichan <- round(chan.dists/vchan+0.5) # vector of length nchan

  # number of outlet considered
  nout <- ncol(routing)-1 #length(ichan)

  # time shift (in simulation steps) for channel flow to reach outlet
	# create a matrix that can hold delays up to the maximum time
  nshift.chan <- max(dtichan)
  chan.routing <- matrix(ncol=nout,
                        nrow= nshift.chan) # enough to distribute all channel flows into future time steps

  chan.routing[] <- 0

  for(iout in 1:nout)
  {
    props <- routing[,iout+1]
  	for(i in 1:length(props))
  	{
  		# sum proportions of channel flow entering at future time step
  		chan.routing[dtichan[i], iout] <- chan.routing[dtichan[i], iout]+props[i]
  	}
    # route all flow to a particular future time step, but could distribute between steps for longer intervals and reach lengths
   # chan.routing[dtichan[chan], chan] <- routing[,2] # second column is proprtion of flow delayed by this time step
  }


  # mulitply the channel flows at a particular time step by the cahhnel routing matrix to get a time-shifted version
  # rewlative to current stimulation step
  # for two channel system this could be something like
  # 0   0
  # 1   0
  # 0   1
  # 0   0
  # ..
  # of flows moved through the putlet that can be added to existing river flows

  return(chan.routing)
}


# route both overland flow Qof and channel flows to outlet
route.channel.flows <- function (groups,
								flows,  # updated flows
								stores,
                delays = NULL, # precalculated time delay matrix, 1 column per channel
								w,        # flux distribution matrix
								Qr,      # time series of simulated total discharge at catchment outlet at simulation time
								it,     # current time step
                dt,      # time step in hours
								Qovf=0,
								ichan=1,
								i.out=1,
								chan.store=0,
								chan.max=180000,
								ob.fact=3,
								chan.dist.matrix=NULL  # option distribution of land
                         		)      # channel id(s)
{
	if(is.null(delays))
	{
		#  total flux transferred out of channel across time step
		Qr[it,] <- flows[ichan[i.out],]$qbf *groups[ichan[i.out],]$area
	}
	else
	{
		# total runoff into channel (include overland flow)
		qchan <- flows[ichan,]$qin + as.numeric(Qovf)
		if(!is.finite(qchan))
		{
			warning("Bad in channel flow, setting to zero")
			qchan <- 0
		}
		else if(sum(qchan)>0)
  	{
      if(!is.null(chan.dist.matrix))
      {
        Qchan <- w[,ichan] * flows$qbf*groups$area
        # split input into channel according to distribution matrix
        Qreach <- t(Qchan[-ichan]) %*% chan.dist.matrix
        Qr[it,2:ncol(Qr)] <- Qreach
      }

  		# determine fluxes at future times passing through each outlet reach specified. colsums may not all = 1
      # as much of the flow will not pass through points upstream. The path length analysis will have taken this account
      # when constructing the distance table supplied vi athe routing parameter
  		nout <- ncol(delays)   # number of outlet reaches specified
  		
  		q.shift <- delays * qchan
  		
  		# how many time steps will it take all the flux entering the channel to reach the outlet (x dt to get actual times)
  		n.shift <- nrow(delays)
  		
#   		if(qchan>60000)  #chan.store > chan.max)
#   		{
# 				# split flow so that somegoes overbank and takes twice as long to get there
#   			
#   			q.shift <- q.shift[rep(1:n.shift, each=ob.fact)]/ob.fact
#   			n.shift <- n.shift*ob.fact
#   		#	Qr[it:(it+n.shift.ob-1),1:nout] <- Qr[it:(it+n.shift.ob-1),1:nout] + q.shift.ob
#   			
#   		}

       # add time shifted fluxes for every channel to the time series. (flow delayed by at least one time step?)
	    Qr[it:(it+n.shift-1),1:nout] <- Qr[it:(it+n.shift-1),1:nout] + q.shift
	    
  	}

  }

  return(Qr)
}


# route both overland flow Qof to the channel, thius time with a single mean distanmce per HRU
# it is then added to the channel input and routed 
route_ovf_zonal <- function (groups,
                             stores,         # storage excess
                             delays,         # time delay histogram for channel flows 
                             routing=data.frame(rep(0, nrow(groups))), # routing table, dist - proportion. Default is for all flow to be routed to the channel in one time step
                             Qr,         # time series of simulated total discharge at catchment outlet at simulation time
                             it,         # current time step
                             dt,         # time step in hours
                             ichan=1,
                             i.out=1)
  # channel id(s)
{
  # total saturation excess storage
  Ex <- sum(stores$ex[-ichan]*groups$area[-ichan])
  
  if(Ex > 0)
  {
    browser()
    # average delays for this storage (if assumed evenly distributed over catchment)
    vchan <- mean(groups$vchan, na.rm=TRUE)
    
    for(i in 1:length(routing$flow.len))
    {
      # calculate number of time step shifts for each proportion of flow
      n.shift.ovf <- round(routing$flow.len[i]/dt)
      
      
      # shift due to channel flows (split the flow and shift each proportion)
      ovf.shift <- Ex*routing$prop[i]*delays
      
      # insert the time steps waiting for the flow to arrive at the channel  
      ovf.shift <- c(rep(0, n.shift.ovf), ovf.shift)
      n.shift <- length(ovf.shift)
      
      # add time shifted fluxes for every channel to the time series. (flow delayed by at least one time step?)
      Qr[it:(it+n.shift-1),i.out] <- Qr[it:(it+n.shift-1),i.out] + ovf.shift
      
    }
    
  }
  return(Qr)
}

# route overland flow to the channel, add to channel input and route to outlet with channel routing velocity
route_ovf_time_delay <- function (groups,
                                  stores,         # storage excess
                                  delays,         # time delay histogram for channel flows 
                                  routing=data.frame(flow.len=0,prop=1), # routing table, dist - proportion. Default is for all flow to be routed to the channel in one time step
                                  Qr,         # time series of simulated total discharge at catchment outlet at simulation time
                                  it,         # current time step
                                  dt,         # time step in hours
                                  ichan=1,
                                  i.out=1)
                                  # channel id(s)
{
  # total saturation excess storage
  Ex <- sum(stores$ex[-ichan]*groups$area[-ichan])
  
  if(Ex > 0)
  {
    # get a value for the flow velocity from the areas producing OVF
    isat <- which(stores$ex[-ichan]>0)
    
    # the velocity is weighted in some part because of the group area
    vof <- min(groups[-ichan,]$vof[isat]) #weighted.mean(groups$vof[isat], groups$area[isat], na.rm=TRUE) 
    # average delays for this storage (if assumed evenly distributed over catchment)
 
    for(i in which(routing$prop>0))
    {
      # calculate number of time step shifts for each proportion of flow ovetlamd to channel
      n.shift.ovf <- round(routing$flow.len[i]/vof)
      
      # shift within channel (split the flow and shift each proportion)
      # convert excess to a flow / unit time
      ovf.shift <- Ex/dt*routing$prop[i]*delays  
      
      # insert the time steps waiting for the flow to arrive at the channel to obtain total delay for
      # eacjh proportion of overland flow
      ovf.shift <- c(rep(0, n.shift.ovf), ovf.shift)
      n.shift <- length(ovf.shift)
      
     # if(it+n.shift-1 > nrow(Qr)){browser()}
      it.shift <- min(it+n.shift-1, nrow(Qr))
      iseq <- it:it.shift 
      
      len <- length(iseq)

      # add time shifted fluxes for every channel to the time series. (flow delayed by at least one time step?)
      try(Qr[iseq,i.out] <- Qr[iseq,i.out] + ovf.shift[1:len])
      
    }
      
  }
  return(Qr)
}

# route.channel.flux.to.outlet <- function(Qreach, drn, pt, dt)
# {
#   # Qreach is total flux over time step into each reach
#   # specific flux (per m) per hour
#   reach.lengths <- gLength(drn, byid=TRUE)
#   qreach <- t(apply(Qreach, MARGIN=1, function(x)x/reach.lengths))
#   qreach <- xts(qreach, order.by=index(run$Qr)[1:nrow(Qreach)])
#
#
# }





