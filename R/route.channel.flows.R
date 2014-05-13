route.channel.flows <-
function (groups, 
																 	flows,  # updated flows
																 	routing, # channel routing table or precalculated time delay matrix, 1 column per channel																 	
																  w,        # flux distribution matrix
																 	Qr,      # time series of simulated total discharge at catchment outlet at simulation time 					 
																 	it,     # current time step
                        					dt,      # time step in hours 	
																 	delays=T,  # is the routing table a calculated matrix of time delays and proportion of flux or a table of distances  
                         					ichan=1, i.out=ichan[1])      # channel id(s)
{
	if(is.null(routing))
	{
		#  total flux transferred out of channel across time step 
		Qr[it,] <- flows[ichan[i.out],]$qbf *groups[ichan[i.out],]$area
	}
	else
	{
	#	if(sum(flows[ichan,]$qof)>0){browser()}
		# input flows into channel: subsurface & surface (overland), converted to a total flow
		qchan <- flows[ichan[i.out],]$qin + flows[ichan,]$qof*groups[ichan,]$area
  	if(sum(qchan)>0)
  	{
  		if(!delays)
  		{
  			# this measn that we have a routing table, first column is distance to outlet (have to amend this)
  			# matrix of flux proportions at time delays (integer step) up to maximum. zero if no flux distributed to that time step  
  			routing <- get.routing(groups, 
  														 w, 
  														 routing,  
  														 ichan,	dt)
  		}
  		# determine fluxes at future times passing through outlet, distributing the total channel input 
  		# according to the proportions given in the 
  		
  		q.shift <- routing %*% qchan
  		# how many time steps will it take all the flux entering the channel to reach the outlet (x dt to get actual times)
  		n.shift <- nrow(routing)
       # add time shifted fluxes for every channel to the time series. (flow delayed by at least one time step?)
	    Qr[it:(it+n.shift-1)] <- Qr[it:(it+n.shift-1)] + q.shift 	    	
  	}
  }
  
  return(Qr) 
}
