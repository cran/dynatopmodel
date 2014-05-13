get.routing <-
function(groups, 
                        w, 
												routing,   # channel routing table, first column is average distance to outlet, second the proportion of flow entering at this upstream distance
                        ichan,      # channel identifiers
                        dt       # time step (hr)										                                                        
                       )
{
	# apply average channel wave velocity
	vchan <- mean(groups[ichan,]$vchan)
	chan.dists <- routing[,1]
  groups <- groups[-ichan,]
    
  # time delay for flow once in channel(s) until reaching outlet 
  # assumming a constant channel velocity
  dtichan <- round(chan.dists/vchan+0.5) # vector of length nchan 
  #nshift <- max(dtiof) + max(dtichan)
  
  nchan <- length(ichan)
  ngroup <- nrow(groups)
  # time shift (in simulation steps) for channel flow to reach outlet
	# create a matrix that can hold delays up to the maximum time
  nshift.chan <- max(dtichan)
  chan.routing <- matrix(ncol=nchan, 
                        nrow= nshift.chan) # enough to distribute all channel flows into future time steps

  chan.routing[] <- 0  
	props <- routing[,2]
  for(chan in ichan)
  {
  	for(i in 1:length(props))
  	{
  		# sum proportions of channel flow entering at future time step
  		chan.routing[dtichan[i], chan] <- chan.routing[dtichan[i], chan]+props[i]
  	}
    # route all flow to a particular future time step, but could distribute between steps for longer intervals and reach lengths
   # chan.routing[dtichan[chan], chan] <- routing[,2] # second column is proprtion of flow delayed by this time step
  }

	# overland flow routing
	if(FALSE)
	{
		# how are the destinations of overland flow from each group distributed between channels?
		# flow weighting matrix determines flux transfer at each step, run a huge number
		# until all flux transferred to the river. flow recirculated within reaches once in river  -
		# river routing logic will transfer this to through the outlet and remove from fluxes
		chandest<- round(matrix.power(w, 1e4)[-ichan,ichan], 3)  # nchan x ngroup matrix (groups here exclude channel HSUS)
				
		# time delay (in terms of number simulation steps) for overland flow fropm each group
		# to reach channel, assumming constant overland flow velocity
		dtiof <- pmax(round(groups$dchan/vof+0.5),1)  # at least one time step to route flow
		
	  # similarly for overland flow from each group - get time sfift for flow from each group to reach channel
	  nshift.land <- max(dtiof)
	  
	  landrouting <- matrix(ncol=ngroup, nrow=nshift.land)
	  landrouting[] <- 0
	  for(igroup in 1:ngroup)
	  {
	    landrouting[dtiof[igroup], igroup] <- 1  # 10 x ngroup    
	  }
	  
	  # landrouting %*% qof gives a time shifted channel flux distribution with each row the fluxes  
	  # distributed from the land into each channel at the corresponding forward time step 
	  # to get final time-shifted distribution apply the chanrouting. Total time shift is
	  # sum of the row and columns of each 
	  	  
	  # result is nshift x nshift matrix that routes all overland flow to outlet
	  # this gives the time shift for each channel
	  landshift <-  landrouting  %*% chandest# nchan x ngroup * ngroup  x 10 => nchan x 10
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
  
  
  # multiply overland flow (total generated in time step) by the diustribution matrix
  # to get a time-shifted vector of discharges 
  return(chan.routing)  #list("chan"=chanrouting, "land"=landrouting))
}
