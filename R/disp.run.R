disp.run <-
function(run, 
                             qmax=NA, legend=F,
                             title = "",
                             disp.par = def.disp.par(),
                             ...)
{
  par(family="serif")
  disp.par$legend.show <- legend
  disp.par$title.main<- title
  qmax <- 1000*max(c(max(run$qsim), max(run$qobs), qmax), na.rm=T)*1.25
  disp.discharge.selection(qsim=run$qsim, evap=run$evap[,"ae"], rain=run$rain, qobs= run$qobs,
                            qmax=qmax,disp.par=disp.par,...)
  #, run.par=run.par)
  
  
}
