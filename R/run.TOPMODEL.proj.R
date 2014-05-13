run.TOPMODEL.proj <-
function(proj, i.disc=1, disc=NULL, ncuts=15, nreach=5,...)
{   
  if(is.null(disc)){disc<-proj$disc[[i.disc]]}
  # note that rain is per time step, not a rate, so will have to be scaled 
  
  params <- colMeans(disc$groups[-disc$ichan,c("m", "srz_max", "ln_t0", "td", "srz0", "k0", "CD")], na.rm=T)
  params <- c(params, "vchan"=mean(disc$groups[disc$ichan,"vchan"], na.rm=T))
  
  # note that TOPMODEL expects input rainfall and pe  per timestep., not as hourly rate
  rain <- proj$obs$rain/proj$dt

  nms <- names(params)
  params <- matrix(params, nrow=1)
  colnames(params)<-nms
  routing <- proj$routing

  topidx.frame <- disc$topidx.frame

  if(length(proj$obs$pe)==0)
  {
    proj$obs$pe <- proj$obs$rain
    proj$obs$pe[]<-0
  }
  
  pe <- proj$obs$pe/proj$dt
  
  vchan <- disc$groups
  riv <- disc$channels[[1]]
  riv[which(is.na(riv[]))]<-0
  if(is.null(proj$qt0))
  {
    warning("initial subsurface flow should be supplied. - set to 1e-5")
    proj$qt0<-1e-5
  }
  res <- RunTOPMODEL(proj$dem, 
                     riv=riv,
                     delays=routing,
                     topidx.frame=topidx.frame,
                     rain=rain, 
                     params=params, 
                     qt0=proj$qt0, 
                     dt=proj$dt, 
                     et0=pe, 
                     qobs=proj$obs$qobs,
                     nreach=nreach,...)
  # coerce result into same structrure as dynamic TOPMODEL run


  return(res)
}
