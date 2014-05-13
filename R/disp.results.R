disp.results <-
function (it,  # current time step
                            tm,  # current simulation time
                            qr,  # calculated discharge at outlet
                            rain,   
                            evap,  # actula evapotranspiration
                            groups,
                            flows,
                            stores,  
                            maxq=0,
                            qobs=NULL,
                            wb=NULL,
                            ichan=1,   # channel indicators
                            text.out=stdout(),
                            log.msg = "",
                            start, end,
                            disp.par,
                            run.par)

{
  title <- disp.par$"title.main"
  runid <- run.par$"id"
 # start <- run.par$"start"
#  end <- run.par$"end"
  
  # sim.delay expressed as hours, convert to seconds
  start <- start + run.par$"sim.delay"*3600

  # render either graphical output at this time step?
  show.spatial <- 
     disp.par$"graphics.spatial.show" & tm >= start  
  show.graphics <-     
    it%%disp.par$"graphics.interval"==0 & disp.par$"graphics.show" & tm >= start
  fmt <-  disp.par$"time.fmt"
  # time buffer, that is, the length of time around the current observation that should be displayed
  buff <- disp.par$"graphics.window.length"
  
  if(tm >= start & !is.null(text.out))
  {
    # send to console by default,  disp.par$output.text.flows
  #  txt.out <- paste(flows[,disp.par$output.text.flows], sep="\t", )
    cat(format(tm, fmt), " \t", signif(qr[it],2), "mm/hr", "\t\t", log.msg, "\n", file=text.out)   #    
  }
  else{
    # waiting...
    cat(".")
  }
  
  # graphical output, if requested, after specified time interval
  if(show.graphics)
  {       
    dev.set(disp.par$winid)
    # buffer output
    dev.hold()
    layout(get.layout(disp.par))
       
    qr <- GetTimeSeriesInputRange(qr, start, tm, verbose=FALSE)

    # determine y axis range from 
    # (a) actual evapotranspiration
    # (b) any observed discharges
    # (c) max base flow routed through catchment outlet
    # (d) explicitly specified limits
    ymax <- max(max(evap, na.rm=T), qobs[], na.rm=T)
    ymax <- max(c(maxq, qobs[], 0.01), na.rm=T)
    par("mar"=c(5, 4.5, 5, 4.5))
    evap <- evap[,"ae"]
# flows observed and simulated
    disp.discharges(title, runid, Qr=qr, 
                      evap=evap, rain, tm,                         
                      groups, qobs, 
                      wb=wb,
    				          eff=NULL,
                      ymax=ymax,
                      disp.par=disp.par,
                      run.par=run.par,
                      timeBuffer=disp.par$"graphics.window.length",
                      ichan=ichan)
 
    # barplot of subsurface specific storages 
  #  try(disp.stores(groups, stores,ichan=ichan))
	
     dev.flush() 
    # coloured map of catchment indicating levels of saturation
    if(show.spatial)
    {    
      if(disp.par$graphics.spatial.window.id<1)
      {
        # set up window pane
        # window occupying 2/3 of the bottom half of the screen
        SetFigWindow(1/3, 1, 0, 1/2)              
        par(new=T)       
      }
      else
      {
        # separate window
        dev.set(disp.par$graphics.spatial.window.id)        
      }
      dev.hold()
      par("mar"=c(4, 0.5, 4, 0.5))    
      # what is displayed
      spatial.output <- GetSpatialOutput(groups, flows, stores, disp.par, maxq=maxq)
      
      disp.spatial(groups, vals=spatial.output$vals, 
                     nlevels=spatial.output$nlevels,
                     cm=disp.par$cm.map, 
                     time=tm, 
                     main= spatial.output$main, 
                     range.vals = spatial.output$range.vals,
                     ichan=ichan,
                     ramp=spatial.output$ramp,
                     disp.par = disp.par)
      # restore
      SetFigWindow()
      dev.flush()      
    }
  }
}
