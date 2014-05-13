aggregate.qobs <-
function(proj)
{
  qobs <- subset.zoo(proj$qobs, proj$sim.start, proj$sim.end)
  # aggregate observations to same frequecy as simulation
  dt.qobs <- 1/frequency(qobs)/3600
  n.aggr <- proj$dt/dt.qobs
  if(n.aggr>1)
  {
    cat("Aggregating ", nrow(qobs), "observations at intervals of ",  dt.qobs, "hrs to time index of simulation\n")
    if(n.aggr < 1){warning("Observations at lower time resolution than simulation")}
    # aggregation (e.g quarter hour to hourly)
    tms <- seq(proj$sim.start, proj$sim.end, by=proj$dt*3600)
    index.agg <- rep(tms, each=n.aggr)  # index(proj$rain)
    # trim - why needed ?
    index.agg <- index.agg[1:nrow(qobs)]
    qobs.agg <- aggregate(zoo(qobs), by = index.agg, FUN=mean)
    names(qobs.agg)<-names(qobs)  
    qobs <- qobs.agg    
  }
  return(qobs)
}
