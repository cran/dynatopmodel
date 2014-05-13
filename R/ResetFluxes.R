ResetFluxes <-
function(flows, ichan)
{
  # tidy up
  # remove base flows from channel  now they have been allocated
#  flows[ichan, ]$qin <- 0
  # remove overland flows - also allocated in RouteFlow routine
  flows[, c("qof","pex", "ex", "exus")]<- 0
  return(flows)
}
