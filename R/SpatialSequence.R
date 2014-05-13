SpatialSequence <-
function(groups, flows, stores, ichan=1,
                            ae, rain, qobs, qsim,                              
                            s=start(qsim), 
                            e=end(qsim),                            
                            disp.par, title=disp.par$title.main,
                            nframe=5)
{
  # split screen between hydrograph and spatial output (top)
  layout(matrix(c(1,1,1,1,2,2), nrow=3, byrow=TRUE))    
        
  # time points corresponding to points at which frame is shown
  times <- seq(s, e, length.out=nframe)
  dx <- 1/nframe
  for(i in 1:nframe)
  {
    tm <- times[i]
    spatial.output <- GetSpatialOutput(groups, flows[tm], stores[tm], disp.par)
    # poistion frame
    SetFigWindow((i-1)*dx, i*dx, 2/3, 1)              
    par(new=T)  
         
       disp.spatial(groups, vals=spatial.output$vals, 
                      nlevels=spatial.output$nlevels,
                      cm=disp.par$cm.map, 
                      time=tm, 
                      main= spatial.output$main, 
                      range.vals = spatial.output$range.vals,
                      ichan=ichan,
                      ramp=spatial.output$ramp,
                      disp.par = disp.par)
       
         
         
  }
         
  disp.discharge.selection(disp.par, 
                            qsim, ae, rain, 
                            groups, qobs, 
                            s, e,
                            ymax, ichan, title)         
  
  
  
}
