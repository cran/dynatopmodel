init.input <-
function(groups, dt, ntt,
                      weights, rain, pe, reservoirs, routing,
											ichan, i.out=1,
                      qobs,   # specific or absolute discharge
                      qt0,    # initial discharge
                      qmax,   # max discharge to display
                      vof, vchan, 
                      disp.par, run.par, 
                      sim.start, sim.end,
                      calling.env = NULL)
{  
  if(is.null(i.out)){i.out<-1}
  # combine lists, supplied attributes overriding the defaults and missing values being supplied
  # by defaults 
  disp.par <- merge.lists(def.disp.par(), disp.par)  
  run.par <- merge.lists(def.run.par(), run.par)
    # need this for $ notation
  groups<-data.frame(groups)
  runid <- run.par$id

  tz <- "GMT"
  if(length(rain)==0)
  {
    stop("Rainfall input required")
  }
  # default limits of run period inferred from time series input
  sim.start <- max(start(rain), as.POSIXct(sim.start), na.rm=T)
  sim.end <- min(end(rain), as.POSIXct(sim.end), na.rm=T)

#  end <- min(end, end(rain), na.rm=T)
  
 # simStart <-  start + run.par$"sim.delay"*3600  # add simulation delay (expressed in hours)
  dispStart <- sim.start + disp.par$"graphics.delay"*3600 # hours -> seconds
  
  # graphics interval is in hours but needs to be converted if time step is not
  disp.par$graphics.interval <- round(disp.par$graphics.interval/dt)
  
  # attempt to create the specified location for saved graphics, if required
  if(disp.par$graphics.save & !exists(disp.par$graphics.out))
  {
    dir.create(disp.par$graphics.out)
  }
  
  # check that the river reach distribution is of the correct dimensions -
  # should be ngroup x nreach, where nth row determines the distribution of the
  # nth group into the river network
  ngroups <- nrow(groups)
  # number of river reaches determined from flow length distribution - each row 
  # represents a reach length and proportion of channel within that reach
  # nreach <- nrow(routing)
  
  # inner time step
  dtt <- dt/ntt
  
  nchan <- length(ichan)
  # channel identifiers
#  ichan <- 1:nchan
  weights <- as.matrix(weights)
  # logging output (defaults to console if not specified)
  if(nchar(run.par$log.out)>0)
  {
 #   glogout <- file(run.par$log.out)
    # route messages to a file, otherwise sent straight to console
   # sink(run.par$log.out, type="message", append=T)
  }
  
  # check consistent matrix dims, array sizes and values
  # check times series input in correct format and duration
  start.time <- Sys.time()
  
  LogEvent(paste0("Run", run.par$id, " started"))
  
  # times for output series, dt in hours
  times <- seq(sim.start, sim.end, by = 3600*dt)
  nmax <- length(times)

  # create dummy pe and qobs if none supplied, Q) can be used to set absolute discharge max limit
  if(is.null(pe)){
    pe <- xts(rep(0,nmax), order.by=times)
  }
  # locate and fix any NA records
  pe[which(is.na(pe))] <-0

  qobs <- GetTimeSeriesInputRange(qobs, sim.start, sim.end, verbose=FALSE)

  # dummy series if nothing supplied or outside range of simulation
  if(length(qobs)==0)
  {
    qobs <- xts(rep(0,nmax), order.by=times)
  }

  # as above...
  if(max(qobs[,1], na.rm=T)>0.5)  # a very unlikely value (0.5m/hr!)
  {
    warning("Observed flows converted from mm/hr to m/hr: check that specfic discharge has been supplied")
    qobs <- qobs/1000
  }   
  # use the overland velocity as proxy for channel routing velocity
 # def.chan.par$vchan <- vchan
  # initialise storages and deficits for each group add static parameters to
  # areal groups if a soils map provided then fix up the soil parameters from
  # here, otherwise continue to use default
  groups <- init.hru(groups, ichan=ichan, 
  													 chan.par=def.chan.par(list("vchan"=vchan)), rain=rain)
  # Input rainfall, observed flows and p.e.
  rain <- GetTimeSeriesInputRange(rain, sim.start, sim.end, verbose=FALSE)
  pe <- GetTimeSeriesInputRange(pe, sim.start, sim.end, verbose=FALSE) 
  # add the actual evap
  evap <- cbind("pe"=pe, "ae"=NA)

  try(check.and.fix.input(environment()))

	# calculated time delay matrix for channel routing
	routing <- get.routing(groups, 
											 weights, 
											 routing,  
											 ichan,	dt)
  # max number of time steps that river flux can be shifted
  # Time series of total output flows from output reach(s). add enough extra elements to hold time delayed flows
  n.shift <- nrow(routing)
	times.ex <-seq(sim.start, sim.end+nrow(routing)*dt*3600, by = 3600*dt)

	Qr <- xts(matrix(0,nrow=nmax+nrow(routing), ncol=length(i.out)), order.by=times.ex)
# total overland flow contribution
  Qof <- xts(matrix(0,nrow=nmax,ncol=1), order.by=times)
	colnames(Qr)<- groups[ichan[i.out],]$id
  
  #*****************************************************************************
  # initialisation of flows, storages and deficits for each areal group flows 
  # (at time step t, per plan area except where stated)
  LogEvent("... initialising fluxes and storages...\n")
  
  # can determine a theorectical max for discharge: set all base flow to max ,
  # controlled by saturation transmissivity (z=0)  add in any rainfall that 
  # could be transferred from overland flow, distribute to channel by flux routing matrix,
  # then route to outlet by time delay table
  # see discussion of TOPMODEL theory in Beven (2012), Chapter 6, Box 6.1, p210
  # gamma is the average soil-topographic index defined in  Beven (1986a)
  # typical values for ln_t0 are in range (-7,8). 
  # Could examine catchment and its observed discharges to calibrate this
  groups$qbf_max <- exp(groups$ln_t0-groups$atb.bar)

  if(is.null(qmax))
  {
    satqbf <- data.frame("qbf"=groups$max.qbf)
    # distribute to channel - assume all flow routed through outlet in one time step
    maxQrin <- dist.flux(groups, satqbf, 
                                       ichan = ichan,
                                       W=weights)[ichan,]$qin
    # add in the maximum rainfall - could be distributed to channel 
    qmax <- max(rain[,1], na.rm=T)/2 + dt*maxQrin/sum(groups$area)   # #max(rain, na.rm=T) + 
  }
 # weights <- spam::as.spam(weights)
  
  # in the summer, effective rainfall recharge will be affected significantly be ea.
  # as we don't know the storages yet, apply a 
    
  # estimate for initial base fluxes given a steady state 
  flows <- init.fluxes(groups=groups,  
                      weights,           # flux distribution matrix
                      ichan,       # channel identifiers                  
                      qt0,         # initial recharge / discharge, assummed equal in steady state, taking into account evapotranspiration  
                      dtt)         
  
  # storage deficits estimated from relationship with base flows estimated above
  stores <- init.stores(groups, flows, ichan,
                         qt0) 

  storage.in <- current.storage(groups, stores, ichan)  
  # initial determined from storages vs. deficit
  
  # index in time series after which to start showing results
  i.start <- which(times>=sim.start)[1]
  if(is.na(i.start))
  {stop(paste("Error determining start of simulation period ", dispStart))}
  # 	LogEvent(paste("initialising time series: start ", as.character(start)))
      
  
  # structure to save the average storage deficit. ae, river input, water balance etc etc
  dat <- data.frame("ae"=rep(0,nmax)) 
                   # "wb"=rep(0,nmax), 
                    #"qr"=rep(0,nmax),
                    #"qriv.in"=rep(0,nmax),   # riveer input fluxes
                    #"ae"=rep(0,nmax))
  data.out <- xts(dat, order.by=times)
  
  qriver <- xts(matrix(rep(0,nmax,each=nchan), ncol=nchan), order.by=times)
  hsus <- groups[-ichan,]
  nhsu <- nrow(hsus)

  fluxes <- array(dim=c(nmax, ngroups, 7), dimnames=c( "it", "hsu", "flux"))  
  storages <- array(dim=c(nmax, ngroups, 3), dimnames=c( "it", "hsu", "store"))
  # graphics interval expressed in hours: convert to time steps
  
  # start of outer time stepping logic using a time step at each rainfall observation  
  time<- sim.start
  it <- 1

  # initialise excess (overland) flow to 0
  q.ex <- rep(0, nrow(groups))

  if(!(disp.par$text.out == "N"))
  {
  	text.out <- stdout()
  } 
  else
  {
  	text.out <- NULL
  }
  if(nchar(disp.par$text.out)>0)
  {
  #  file specified
 #   text.out <- file(disp.par$text.out)
  }  
# initialisation
    # run this number of time steps to get an equilibrium with 
  # rainfall and specified initial discharge
  # number of time steps to wait before start
  n.step <- round(run.par$sim.delay/dt)
  LogEvent(paste0("Initialising run for ", run.par$sim.delay, " hours\n"))
  init <- DoUpdateBaseFlows(groups, flows, stores, weights, qt0=qt0, ntts=ntt,
                     nstep=n.step, dt=dt, ichan=ichan, debug=F)
  
  flows <- init$flows
  stores <- init$stores
  # set  first discharge estimates to value specified (total, not specific)
  # extend into lag period whilst flux is stil in transit. If this isn't done then it appears that
  # there is a period when no flux enters and we see a initial dip in the hydrograph
  Qr[it:(it+n.shift),]<- qt0*sum(groups$area)

# set channel deficts to a typical value 
	stores[ichan,]$sd <- groups[ichan, ]$sd_max/2
  run.par$sim.delay <- 0

	# initialise surface excess storage to zero
	surf.ex <- stores$ex
	pe.dist <- xts(order.by=index(pe), matrix(rep(pe, ngroups), ncol=ngroups))

  # open graphics windows
  disp.par <-setup.output(disp.par, run.par)

	# detention storage
#	reservoirs <- build.reservoirs(reservoirs, groups)
	cur.env <-  environment()
  # copy values to the specified environment, typicaly the  current environment in 
  # the calling context
  if(!is.null(calling.env))
  {
    for(n in ls(cur.env, all.names=TRUE))
    {
      # assign name-value in this envir to that specified
      assign(n, get(n, cur.env), calling.env)
    }
  }

  return(cur.env)
}
