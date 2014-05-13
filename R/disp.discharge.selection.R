disp.discharge.selection <-
function(groups, 
                                      qsim,  # in m/hr
                                      qobs,  #  "
                                      rain, evap,
                                      sel=NULL, 
                                      qmax=0,   # upper bound for discharge display, in mm/hr
                                      ichan=1,
                                      disp.par=def.disp.par, 
                                      title=disp.par$title.main,...
                                      )
  
{
  # plot full range
#  if(e<=s){stop("invalid time selection")}
  if(is.null(sel))
  {
    sel<-range(index(qsim))
  }
  
  buf <- as.numeric(difftime(sel[2], sel[1], units="hours")) #[2]-sel[1]
  #  difftime(end, start, end,units="hours")
  # calculate R for this period
#  sel <- sel<-paste(s, "::", e, sep="")
  #qobs <- qobs[sel]
  ae <- xts(1000*subset.zoo(evap, sel[1], sel[2]))
  if(!is.null(qobs))
  {
    qobs <- xts(1000*subset.zoo(qobs, sel[1], sel[2]))}
  qsim <- xts(1000*subset.zoo(qsim, sel[1], sel[2]))

  mar <- c(4, 4, 5, 5)
  if(!disp.par$legend.show)
  {  
    # less space at base
  #  mar[1]<-2
  }
  if(title=="")
  {
    # don't need so much space at top
    mar[3]<-3
  }
    par("mar"=mar)
 
  if(!is.null(qobs))
  {
    #names(qobs)<-"Observed discharges"
    
  }
  #ae <- 1000*evap   #[,"ae"]
  
  disp.discharges(title=title, disp.par=disp.par, runid=NULL, 
                    qsim, ae, 1000*rain, tm=sel[1], 
                    groups, qobs, #eff=eff,
                    ymax=qmax,
                    
                                 # show to end of simulation period
                    timeBuffer= buf, 
                    ichan=ichan,...)  
  
}
