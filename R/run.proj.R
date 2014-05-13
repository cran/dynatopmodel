run.proj <-
function( 
  proj,        # loaded project
  idisc=1,     # discretisation number to run
  disc=NULL,   # or expolicitly supplied (override idisc)
  # some stuff that can be overriden without changing the project
  qobs=NA,   # setting to null suppresses observations from run                      
  rain=NULL,
  start=NULL,
  end=NULL,
  disp=F,          # display output after simulation 
  show.stats=T,    # whether to show effciency measures etc
  run.tm=F,       # TOPMODEL run
  ...)             # any other to override  project settings
{  	
  proj <- as.list(proj)
  proj <- merge.lists(proj, list(...))  # add in optional values
  # simulation bounds either supplied or taken from project settings. convert to date / time objects
  if(!is.null(start)){
    proj$sim.start <- start
  }  
  if(!is.null(end)){
    proj$sim.end <- end
  }    
  proj$sim.start <- as.POSIXct(proj$sim.start)
  proj$sim.end <- as.POSIXct(proj$sim.end)
  
  # note observed flows required in specific discharge m/hr
  if(!is.null(qobs))
  {
    if(!is.na(qobs))
    {
      proj$obs$qobs <- qobs    
    }  
  }
  else{
    proj$obs$qobs <- NULL
  }
  
  if(!is.null(rain)){proj$obs$rain <- rain}
  # must have some rainfall, other observation optional
  proj$obs <- aggregate.obs(proj)
  rain <- proj$obs$rain
  qobs <- proj$obs$qobs
  
  qt0 <- proj$qt0  	
  if(is.null(qt0) & !is.null(qobs))
  {    
    # set an initial discharge, assummed steady state and equal to rainfall recharge
    qt0 <- as.numeric(qobs[1,1])
  }  
  # either supply discretisation explicitly or index in the project list
  if(is.null(disc)){
    disc <- proj$disc[[idisc]]
  }
  qsim<-NULL  
  if(run.tm)
  {
    # obsertvation already aggreageted to desired time step
    run.sim <- run.TOPMODEL.proj(proj, disc=disc)
  }
  else
  {      
    try
    (
      # call the dynamic TOPMODEL
      run.sim <- run.dtm(qobs=qobs, 
                            groups=disc$groups,
                            rain=rain, # require 
                            weights=disc$w,  
                            pe=proj$obs$pe,
                            ichan=disc$ichan,
                            i.out=proj$i.out,  # outlet reach id
                            ntt=proj$ntt,
                            qt0=qt0,
                            qmax=proj$qmax,
                            vchan=proj$vchan,
                            vof=proj$vof,
                            dt=proj$dt,      # simulation time step in hours
                            sim.start=proj$sim.start,
                            sim.end=proj$sim.end,
      											routing=disc$routing,
                            reservoirs=disc$reservoirs,
                            disp.par= proj$disp.par,
                            run.par = proj$run.par)
    )
    
  #  proj$groups<-run.sim$groups
  }

  
  if(show.stats)
  {
    cat("Elapsed run time = ", format(run.sim$dur, digits=3), "\n")
    cat("Water balance = ", signif(run.sim$wb,2), "m\n")
    try(show.gof(proj, run.sim))
  }
  if(disp)
  {
    disp.run(run.sim, title = proj$disp.par$title.main)
  }
  return(run.sim)

}
