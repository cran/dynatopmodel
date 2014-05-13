get.mor.proj <-
function(data.dir=fp(Sys.getenv("DATA_DIR"), "catchment/eden/morland"))
{
  
  mor <- create.proj(data.dir)
  
  #mor<-apply.params(mor, list(m=0.009, ln_t0=14.5, srz0=1, srz_max=0.05, vchan=1500))
  mor$dt<-0.25
  # convert
  mor$obs$qobs <- convert.to.specific.discharges(mor, mor$obs$qobs)
  mor$obs$rain <- mor$obs$rain/1000/mor$dt
  
  mor$sim.start <- "2012-10-27"
  mor$sim.end <- "2013-01-20"
  
  mor$qt0 <- as.numeric(mor$obs$qobs[mor$sim.start,])[1]
  mor$obs$pe <- with(mor, approx.pe.ts(sim.start, sim.end, dt=mor$dt, 
                                       emax=2.7/1000))   # this gives an annual total of 480mm//yr
  mor$qmax<- 3/1000
  
  mor <- apply.params(mor, list(srz0=0.99, ln_t0=14.5, vchan=5000, m=0.0062, srz_max=0.05, td=0.5))
  mor$ntt<-5
  colnames(mor$obs$rain) <- "Morland TBR AWS"
  colnames(mor$obs$qobs) <- "Newby"
  mor$title <- "Morland Beck"
  mor$run.id <- "Morland demo run"
  mor$disc <- mor$disc[which(mor$hru.counts==11)]
  return(mor)
}
