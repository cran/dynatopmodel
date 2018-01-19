

# root zone storage, excess flow into unsaturated zone and actual evapotranspiration
# time step dt in hrs
# ==================================================
# Inputs
# --------------------------------------------------
# time step dt in hrs
# group flows and storages
# p.e and rainfall over thjis time step
# ==================================================
# Returns
# --------------------------------------------------
# updated root zone storage, excess flow into unsaturated zone and actual evapotranspiration
# precipitation excess, if any, from root zone into unsaturated zone
# ==================================================

# areal input rainfall = flows$rain
# pe = potential evapotranspiration per unit time (mm/hr)
# flow: current root zone storage
root.zone <- function(groups, flows, stores, pe, dt, time,
                     ichan)
{
  # precipitation excess initially zero
  flows$pex <- 0
  rain <- flows$rain

  # add rainfall input over this time step  (rain is an hourly *rate*)
  # any excess flow (from e.g surface storage) will have already been added
  stores$srz <- stores$srz + dt*rain

  # actual ET for land groups is calculated using ratio of storage to max storage
  # limit to 1 to deal with cases where the root zone has been overfilled by rainfall across this 
  # timestep. AE is then removed at the maximum potential rate.
  fact <- pmin(stores$srz/groups$srz_max, 1)

  # pe and ae also rates
  ae <- pe * fact # * as.numeric(rain < 1e-8) #* dt

	# Limit evap removed from root zone across the time step to amount of storage remaining 
	ae <- pmin(ae, stores$srz/dt)  # convert evap rate to amount over (inner) time step

  # remove ae from the root zone (this should now not drops below)
  stores$srz <- stores$srz - ae*dt    #pmax(, 0)

	# evaporation is removed at maximum allowed from the free surface of the channel
	ae[ichan] <- pe[ichan]

  # Route any excess over max storage into unsaturated zone, where it drains into water table
  ifull <- which(stores$srz>=groups$srz_max)  #setdiff(StoresFull(groups, stores), ichan)
  if(length(ifull)>0)
  {
    # Precipitation excess  routed into the unsat zone 
    flows[ifull,]$pex <- (stores[ifull,]$srz -groups[ifull,]$srz_max)/dt   # note pex is also a rate
    # these root zones are full
    stores[ifull,]$srz <- groups[ifull,]$srz_max
  }
  #

  flows$ae <- ae
#  flows[ichan] <- NA
  return(list("flows"=flows, "stores"=stores))
}

