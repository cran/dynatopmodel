# downslope distribution function resulting from exponential transmissivity
# function relating rate of change of flow to unsaturated drainage r and upslope input and downslope output
# called from ode 
dqdt.ode <- function(t, y, parms, 
                      ...)
{
 # y <- pmin(y, parms$qbmax)
  if(length(parms$idry)>0)
  {
  	y[parms$idry] <- 0	
  }
  
	res <-  y/parms$m * (parms$A %*% y + parms$r)
	
	return(list(res))
}

#cdqdt.ode <- cmpfun(dqdt.ode)

require(deSolve)

# ******************************************************************************
# kinematic.r kinematic wave routes flow downslope through groupings from
# 4-point solution using input flows and output flows from previous and current
# time steps
# ==============================================================================
# Inputs
# ------------------------------------------------------------------------------h
# time step dt in hrs
# flowst1: group flows and storages at previous time step
# flows: groups flows at current time step- Qbf to be determined
# w: time-stepping weighting coefficient: zero for a totally implicit solution
# (depends only on flows for previous steps). w=0.5
# niter: max number of iterations in iterative scheme
# ==============================================================================
# Returns
# ------------------------------------------------------------------------------
# updated storage deficts, estimates for base flows at this time step
# ==============================================================================
# References
# ------------------------------------------------------------------------------
# Beven and Freer (2001). A Dynamic TOPMODEL
# Beven (1981). Kinematic subsurface stormflow
# Li el al (1975).  Li, Simons and Stevens 1975 - Nonlinear Kinematic Wave
# Approximation for Water Routing
# Beven (2012). Rainfall runoff modelling, chapter 5 pp141-150, pp.180-183
# ******************************************************************************
# note: we now exclude lateral input from land hsus from the flux inpu
route.kinematic.euler <- function(groups,
                            flows,        # fluxes at previous time step (prediction time)
                            stores,       # current storage
                            dtt,
                            ichan,
                            time,
							              w,
                            nstep=1,
				                    dqds,
														A=NULL,
                            method="lsoda"              # Livermore solver
)
{
  # UZ: recharge is drainage from unsaturated into saturated zone - assumed constant over time steps
  r <- flows$uz #

 	qb0 <- flows$qbf
 	qb0[ichan] <- 0
 	
 	
 	#	dddt <- q- (t(w)%*%(a*q))/a-r
 	#	dqdt <- dddt*-q/m
 	if(is.null(A))
 	{
 		a <-groups$area
 		#  fun <- parms$dqds
 		
 		N <- nrow(w)
 		
 		 A <- diag(1/a, N, N) %*% t(w) %*% diag(a, N, N) - identity.matrix(N)
 	}
 	
 	# identify any units that have dried out andare not producing any base flow
 	idry <- which(stores$sd>=groups$sd_max)
 	
 	# nstep is the number of results to produce
 	res <- ode(y=qb0, 
 						 times=seq(0, dtt, length.out=nstep+1),
             func=dqdt.ode,
 						 parms=list(A=A,
 						 						idry=idry,
 						 					  m=groups$m,
 						 						qbmax=groups$qbf_max,
 						 						r=r),
 						 method=method)
 	
  # final row gives baseflows at the end stage
 	flows$qbf <- res[nstep+1,-1]

  return(flows)
}





