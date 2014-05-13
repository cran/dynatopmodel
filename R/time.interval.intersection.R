time.interval.intersection <-
function(obs, sim.start, sim.end)
{
  int1 <- intervals::Intervals(range(sim.start, sim.end))
  int2 <- intervals::Intervals(range(index(obs)))  # need xts object
  int3 <- intervals::interval_intersection(int1, int2)
  return(int3) 
}
