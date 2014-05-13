InitialiseChannelFlows <-
function(flowlengths, dt, deltaR, q0, chanV)
{
  # channel flow along river reaches - initially zero - use length factors supplied with
  # channel delays table
  qriver <- flowlengths[1,]

  # qchan is flux per downstream length for the river reaches
  # Q0 is the total flow out of the catchment in 1hr. 
  # the river volume that flows out of the catchment in 1 hr is qchan[1]*chanV*dt
  # As steady-state solve and apply to entire river network - means that narrower reaches
  # have higher fluxes?
  qr <- q0/chanV
  qriver[] <- qr

  # expand into a vector of flows per 1m lengths of the channel
  qriver <- as.vector(matrix(rep(qriver, deltaR),nrow=deltaR,byrow=TRUE))  
  #return(qriver)
}
