setup.output <-
function(disp.par,
                        run.par)
{
  if(disp.par$"graphics.show"==T)
  {
    if(disp.par$"graphics.save"==T & !file.exists(disp.par$"graphics.out"))
    {
      # mmmm
        dir.create(disp.par$"graphics.out", recursive=T) 
    }
    # ensure suffcient windows open to display results
    disp.par$winid <- open.devices(1, title=run.par$id, 
                width=12, #disp.par$graphics.save.width, 
                height=8) #disp.par$graphics.save.height)
    # identify the first window of default type on list with hydrograph output
    
    
    par(family="serif")
    # graphics recording can be expensive in memeory use so disbale for duration of run
    dev.control("inhibit")
    on.exit(dev.control("enable"))
    if(disp.par$graphics.spatial.show & disp.par$graphics.spatial.window.id >1)
    {
      # calculate matrix elevations and axis values for display (update global value!)
      disp.par <- calc.persp.matrix(run.par, disp.par)
      
      disp.par$graphics.spatial.window.id <- open.devices(2, title=run.par$id) 

    }
  }
  # return the updated display parameters
  return(disp.par)
}
