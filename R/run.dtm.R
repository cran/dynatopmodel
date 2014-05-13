run.dtm <-
function(groups,                    # data frame of group definitions                  
                       weights,                   # weighting matrix
                       rain,                      # rainfall record (m/hr)
                       qobs=NULL,                 # observed specific flows (m/hr). Total discharge will be converted                                
                       qt0=NULL,                  # initial specific discharge at outlet (m/hr): total discharge will be converted
                       pe=NULL,                   # potential evapotranpiration (m/hr). set to if not supplied                         
                       dt=1,                      # time step (hrs)
                       ntt=1,                     # number of inner time steps        
					             ichan=1,  	                # channel identifiers
                       i.out=ichan[1],            # output channel identifier
					             vchan = 1000,              # default channel wave velocity m/hr (constant across catchment) 
					             vof = 100,                 # effective overland flow velocity, m/hr
					             qmax = NULL,               #  max specific flow for display (m/hr)
											 routing=NULL,              # routing table comprising flow distance histograms for each HRU. If null then output flows calculated in kinematic routine used
                       reaches=NULL,              # optional channel reach info
											 reservoirs=NULL,           # list of detention storage locations identified with items in groups table
                       sim.start=NA,              # simulation start and end, inferred from input series if not specified
                       sim.end=NA,
                       disp.par = def.disp.par(),  # display parameters are described above. Defaults supplied for missing items
                       run.par = def.run.par()) 
{  
#  enableJIT(3)   # just in time compilation
  start.time <- Sys.time()
  # setup input variable for run using supplied data, and copy the updated values back to 
  # to the current environment
  init.input(groups, dt, ntt, 
  					  weights, rain, pe, reservoirs, routing,
  					  ichan, i.out, qobs, 
  				    qt0=qt0,  qmax=qmax, vof=vof, vchan=vchan,
              disp.par, run.par,                
              sim.start, sim.end,
              calling.env=environment())

  text.out <- stdout()
  storage.in <- current.storage(groups, stores, ichan)
  catch.area <- sum(groups$area)
  
  while(time <= sim.end)
  {          
    # Allocate rainfall to groups depending on gauge specified, then 
    # add in any overland excess distributed downslope in a previous timestep (flux). initalised to zero in init.input
    flows$rain <- as.vector(allocate.rain(rain, groups, it) + flows$qof)
        
    pe.dist <- allocate.PE(pe, groups, it)
   # stores <- SaturatedEvap(groups,flows,stores,peDist)  
    # subsurface flux routing and deficit update
   	updated <- update.subsurface(groups, 
   													flows=flows, stores=stores, weights=weights,                               												   													
                            pe = pe.dist,
                            tm=time, 
                            ntt=ntt, 
                            dt=dt, 
                            ichan=ichan, 
   													reservoirs=reservoirs)
    flows <- updated$flows  
    stores <- updated$stores 

   # overland excess storage will be redistributed to rainfall input for next step
   # and reset at zero for next pass. calculate equivalent overland flow generate dby this amount of
   # excess storage created over the time step
   flows$qof  <- route.ovf(groups, flows, stores, 
   												w=weights, dt=dt, ichan=ichan)/dt
  
     
   # route to outlet or update current, either using output 
   Qr <- route.channel.flows(groups, flows, routing, weights, Qr, it, dt, ichan)
   	
    # save output
   storages[it,,]<- as.matrix(stores[, c("srz", "suz", "sd")])

   fluxes[it,,]<- as.matrix(flows[, c("qbf", "qin", "uz", "rain", "ae", "ex", "qof")])
   
    evap[it,"ae"] <- WeightedAverage(flows$ae, groups$area)
  #  rain <- 1000*WeightedAverage(flows$rain, groups[-ichan,]$area)       
  disp.results(it,          
                     tm=time,     
                     qr=Qr*1000/catch.area, # calculated specific discharge at outlet, mm/hr
                     rain=rain*1000,            # rain and ae converted back to mm/hr                     
                     evap=evap*1000,  # 
                     groups,
                     flows,
                     stores,    
                     maxq=qmax*1000,  # calculated maximum base flow routed through channel (mm/hr)
                     wb= NULL, #output[,"wb"]*1000,
                     qobs*1000,  # this should have been converted to specific discharge. It also is in mm/hr
                     ichan=ichan,   # channel indicators
                     text.out=text.out,
                 #    wb=wb,
                     log.msg="",  #log.msg,
                     start = sim.start,
                     end = sim.end,
                     disp.par,
                     run.par)    


    # overland flow contribution is the total input to the channels (doesn't matter where it ends up after that)
  # not needed? 
	#	flows <- ResetFluxes(flows, ichan)            
		stores <- ResetStorages(stores, ichan) 

    # update index and time step    
    time <- time + dt*3600 
    it <- it+1
 
  }
  # specific discharge
  qr <- Qr/catch.area
  # convert fluxes to a named list
  fluxes <- multi.array.to.list(fluxes,  c("qbf", "qin", "uz", "rain", "ae", "ex", "qof"))
  # specific upslope input 
  fluxes$qin <- as.matrix(fluxes$qin)/groups$area  #fluxes$qin/groups$area
  
  it <- it-1

  qsim <- Qr[1:it,]/catch.area #    subset.zoo(Qr, sim.start, sim.end)  # specific flux
  qobs <- qobs[1:it,] #subset.zoo(qobs, sim.start, sim.end)
  wb <- water.balance(groups, stores, dt=dt, storage.in, ichan, qsim, 
                      rain, ae=fluxes$ae)
	dur <- difftime(Sys.time(), start.time, units="secs")
 
  RunSummary(groups, stores, storage.in, ichan, qsim, qobs, start.time, 
                         rain, ae=evap[,"ae"], disp.par$text.out)
  
	Qof <-  fluxes$qof[,ichan]*groups[ichan,]$area
  # total ovf is amount transferred to outlet (includes rain directly to channel?)
  # in this formulation all excess flow is routed immediately and then removed
  ovf <- dt*sum(Qof)/catch.area
  
  storages <- multi.array.to.list(storages,  c("srz", "suz", "sd"))
 
 # ts <- cbind(qsim=qsim, qobs=qobs, rain=rain, evap=evap)
 # colnames(ts) <- c("qsim", colnames(qobs), "rain", colnames(evap))
  return(list(qobs=qobs, qsim=qsim, rain=rain, evap=evap, 
              wb=wb, fluxes=fluxes, storages=storages,
              dur=dur, catch.area=catch.area, ovf=ovf, dur=dur))
}
