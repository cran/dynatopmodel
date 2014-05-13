route.kinematic <-
function(groups, 
                            flowst1,      # fluxes at previous time step (all known)
                            flows,        # fluxes at current time step (prediction time)
                            stores,       # current storage
                            dtt, 
                            ichan, 
                            time, 
						                w=0.5,                      #  weighting factor in numerical scheme
						                # a function that provdes wave celerities for each groups given fluxes and other parameters
						                celerity.func = exponential.subsurface.linear.channel,  
                            # can supply 
						                nIter=30)                   # maximum no. iteration before non-convergence identified
{
  eps <- 1e-6
 # funs <- 
  # UZ: recharge is drainage from unsaturated into saturated zone - assumed constant over time steps 
  # (supply previous time step's value?)
  r1 <- flowst1$uz  # recharge at previous time step
  r <- flows$uz # r1 # recharge at this time step (probably constant?)
  
  # QINt-1: previous (total) input flow for areal groups - divide through to get flux (not intensity)
  qin1 <- flowst1$qin/groups$area
  # QBF-1: previous output (base) flow (flux per unit area) for groups  
  qbf1 <- flowst1$qbf
  # current input flow -> flux
  qin <- flows$qin/groups$area
  # use the input flow plus recharge over at this time step as an initial estimate for output base flow
  qbf <- qin+r1
   
  # wave celerities within each group - estimate dq/dx=q from average of flows (should be +ve)
  c1 <- celerity.func(qin1, qbf1, groups, ichan) 
  
  # constant value using just the flows at the previous time step to acts as basis 
  # for the iterative solution
  const <- (1-w)*c1*dtt*(qbf1-qin1-r1) #   was r
  
 # which areas to solve for (excludes river - areas with zero flow)
  unsolved <-  setdiff(1:length(qbf), ichan)
  # save each stage of solution
#  qiter <- qbf
  # iterate towards stable solution(s) for the output flows
  for(step in 1:nIter)
  {    
    # celerities proportional to the flow estimate
    ct <- celerity.func(qin, qbf, groups, ichan) 
    
    # save prev soln to compare with next estimate
    qbfPrev <- qbf

    #qbf <- (qbf1 + w*c*dtt*(qin[unsolved]+r) + const)/(1-w*c*dtt)   # qin -> flows$qin
    
    # attempt new solution for qbf at time step 
    qbf[unsolved] <- (qbf1[unsolved] 
                      - w*ct[unsolved]*dtt*(qin[unsolved]+r[unsolved]) + const[unsolved])/(1-w*ct[unsolved]*dtt)   
  #  qbf <- (qbf1 + w*ct*dtt*(qin+r) + const)/(1-w*ct*dtt)   # qin -> flows$qin

    # force flows to be non-negative, or can we have a small negative uphill flow
    
    # updated flows close to previous estimate = solution achieved 
    unsolved <- which(abs(qbf-qbfPrev) > eps)  # CompareFlows(qbf, qbfPrev, eps))
    # save calculated base flows at this stage
  #  qiter <- rbind(qiter, qbf)    
    if(length(unsolved)==0) {break()}  
  }
  if(length(unsolved)>0)
  {
    LogEvent("Non-convergence in kinematic solution")#, groups[unsolved,]$tag), warn=T, tm=time)    
  }
  negFlow <- setdiff(which(qbf<0), ichan)
  if(length(negFlow)>0)
  {
   # browser()
    LogEvent(paste("qbf < 0 in ", length(negFlow), " group(s)"), warn=T, tm=time)
    qbf[negFlow] <- 0
  }
  # max saturated flux is calculated from topography of groups, setting SD=0
  # if we exceed this then set the flux to that value
 #maxqbf <- exp(groups$ln_t0-groups$atb.bar)  #  total flux groups$area* 
  maxsat <- which(qbf>groups$max_qbf)
  if(length(maxsat) > 0)
  { 
    # excess 
    ex <- qbf[maxsat]- groups$max_qbf[maxsat]
 #   LogEvent(paste0("Base flow excess in  ", paste(groups[maxsat,]$tag, collapse=", ")), tm=time) 
        # set to maximum
    qbf[maxsat] <- groups$max_qbf[maxsat]    
    flows$ex <- 0
    # excess added to saturated excess store then to surface flow
    flows[maxsat,]$ex <- ex  
  }
#   #   }  
  #flows$ex <- pmax(0, flows$qbf-groups$qbf_max)
  flows$qbf<- qbf

# updated base flow and storage  
  return(flows)    
}
