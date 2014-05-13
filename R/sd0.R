sd0 <-
function(groups, flows)
{
  stores$sd <- groups$sd_max
  
  # q0  saturated specific areal base flows  estimated from areal average 
  # of topographic index and saturated transmissivity
  q0 <- exp(groups$ln_t0-groups$atb.bar) 
  
  # initial base flows estimated from initial recharge assumming steady state
  # see Beven 2012 equation 6.1.21
  sdbar<- -groups$m * log(flows$qbf/q0)
  
  return(max(sdbar, 0))
}
