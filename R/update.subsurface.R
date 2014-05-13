update.subsurface <-
function (groups, flows, stores, 
													weights,  													
                          pe=0, # potential evap
                          tm,   # current simulation time 
                          ntt,  # no. inner time steps
                          dt,   # main time step
                          ichan=1,
													reservoirs=NULL)   # optional list of detention storage added to channel)
{
  # save storages
  stores1 <- stores
  
  dtt <- dt/ntt

  # subsurface flows for subsurface and channels at inner time steps
  qriv.in <- matrix(0,ncol=length(ichan), nrow=ntt)
  colnames(qriv.in) <- groups[ichan,"id"]  
  qriv.out <- qriv.in
  
  # base flow excess (per inner time step)
 # qb.ex <- matrix(0,ncol=nrow(groups), nrow=ntt)
  # record of actual evapotranpiration over each inner step
  ae.dtt <- matrix(0,ncol=nrow(groups), nrow=ntt)
  timei<-tm
  for(inner in 1:ntt)
  {     
#  	stores1 <- stores
  	# previous time step's flows (base flow and unsat drainage) held in flows
  	updated.flows <- flows
  	
  	# apply rain input and evapotranspiration (*rates*) across the inner time step
    # note that rain and actual evap are maintained in flows
    updated <- root.zone(groups, updated.flows, stores, pe, 
                    dtt, timei, ichan)        #
    # ae is removed from root zone only - note that original DynaTM has code to remove evap
    # at max potential rate from unsat zone
    updated.flows <- updated$flows  # includes ae and rain
  	stores <- updated$stores  #  storage excess
    
    # route excess flow from root zone into unsaturated zone and  drainage into water table
    updated <- unsat.zone(groups, updated.flows, stores, dtt, timei, ichan)
  	updated.flows <- updated$flows
    stores <- updated$stores
     	
    # Distribute baseflows downslope through areas using precalculated inter-group
    # flow weighting matrix to give input flows for at next time step - required for ann implicit soln
    # note total input qin returned andconverted within kinematic routine
    
    # distribute fluxes to give new estimate for qin(t+1) - total input flux at t+1    
    # base flux transferred from other areas across inner time step
    updated.flows$qin <- dist.flux(groups, flows, 
                                           ichan = ichan,
                                           W=weights)

    # 4 point kinematic wave solution (Li, 1975),     
    # see formulation using exponential transmissivity in Beven and Freer (2001)
    flows <- route.kinematic(groups, flows, updated.flows, stores, dtt, ichan=ichan, 
                             time=timei) 
  	
  	# deal with any detention storage added to channel 
  	#flow <- update.reservoir.output.flow(groups, flows, stores, reservoirs)
  	
		# now update stores and route any excess flow
    updated <- update.storages(groups, flows, stores, dtt, ichan, tm=time)
		
		flows <- updated$flows
		# stores updated by net baseflow and drainage from unsat zone across time step
    stores <- updated$stores 

    stores$ex <- stores$ex + flows$ex*dtt
    
    # actual ae at this time step
    ae.dtt[inner,] <- flows$ae
    # total excess over inner time step: sat excess surface storage and base flow excess    
    # record base flow into and out of all river reaches
#    qriv.out[inner,] <- flows[ichan,"qbf"] 
    qriv.in[inner,] <- flows[ichan,"qin"] 

    flows$ex <- 0
    #stores$ex<-0
    timei <- timei + dtt*3600
  }
	
  # average out total ae
  flows$ae <- colMeans(ae.dtt)  #Sums(ae.dtt)*dtt/dt

	# channel flows are rain in over time step minus evapotranspiration, which doesn't vary according 
	# to root zone storage, only whether rain is falling at the time. take mean of total input across inner loop
	flows[ichan,]$qin <- colMeans(qriv.in) + (flows[ichan,]$rain-flows[ichan,]$ae)*dt*groups[ichan,]$area

  # specific overland flow (rate) 
  flows$qof <- stores$ex/dt

	# ############################## water balance check ###########################
# 	stores.diff <- stores - stores0
# 	store.gain <- stores.diff$ex + stores.diff$srz + stores.diff$suz-stores.diff$sd
# 	net.in <- (flows$rain- flows$ae)*dtt
# 	bal <- store.gain-net.in
	# ############################## water balance check ###########################
  # return updated fluxes and storages, total discharge into river
  return(list("flows"=flows, 
              "stores"=stores))  			
}
