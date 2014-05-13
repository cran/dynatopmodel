check.time.intervals <-
function(proj)
{
  int <- time.interval.intersection(proj$obs$rain, 
                                    as.POSIXct(proj$sim.start), as.POSIXct(proj$sim.end)) 
  return(intervals::size(int)>0)
}
