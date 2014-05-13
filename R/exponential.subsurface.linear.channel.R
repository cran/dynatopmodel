exponential.subsurface.linear.channel <-
function(qin, qbf, params, ichan) 
{
  # z = D/eta  D=eta*z
  # set f=eta/m
  # c = -1/eta * dq/dz 
  # q = T0*tanb exp(-fz) => dq/dz = T0tanB*-f *exp(-fz)=-fq
  # so   c = -eta/m *q *1/eta * q = -q/m
  cel <- -(abs(qin+qbf)/(2*params$m))
  # Fixed channel velocities. note -ve as deficit moves upstream as flood wave moves downstream
  cel[ichan] <- -params[ichan,]$vchan
  
  return(cel)   # szm is >0 Celerity is always in +ve x direction 
}
