aggregate.obs <-
function(proj)
{
  try(proj<-fix.run.dates(proj), silent=T)
  obs <- proj$obs
  # check that the specified start end and end dates contain at least some rainfall data. Other data are
  # less important and take null defaults if not specified
  try(obs$pe <- disaggregate.xts(proj$obs$pe,
                                 ser.start=proj$sim.start,
                                 ser.end=proj$sim.end,
                                 dt=proj$dt, is.rate=T))
  try(obs$rain <- disaggregate.xts(proj$obs$rain,
                                   ser.start=proj$sim.start,
                                   ser.end=proj$sim.end,
                                   dt=proj$dt, is.rate=T))
  # note observed flows required in specific discharge m/hr
  try(obs$qobs <- disaggregate.xts(proj$obs$qobs,
                                   ser.start=proj$sim.start,
                                   ser.end=proj$sim.end,
                                   dt=proj$dt, is.rate=T))
  
  return(obs)
}
