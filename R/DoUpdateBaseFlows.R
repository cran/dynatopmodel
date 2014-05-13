DoUpdateBaseFlows <-
function (groups, flows, stores, weights, qt0, 
                               tm=Sys.time(), nstep=10, dt=1, ntts=2, 
                               ichan=1, i.outlet=1,
                               debug=F)
{
  flows$uz <- qt0
  flows[ichan,]$uz<-0
  bals <- NULL
  qrivs <- NULL
  orig.flows <- flows
  orig.stores <- stores
  flows$rain <- qt0
  pe0 <- flows$qin
  pe0[] <- 0
  for(ntt in ntts)
  {
    bal <- NULL
    flows <- orig.flows
    stores <- orig.stores
    qbfs <- flows[-ichan,]$qbfs
    qins <- NULL
    deficits <- stores[-ichan,]$sd
    qriv <- NULL
    recharge <- NULL
    storage.gain <- NULL
    
 #   cat("ntt=", ntt, "\n")  
  # fix drainage / rainfall at desired discharge and run repeatedly until it settles down
    flows$rain<-qt0
 #   pb<-txtProgressBar(0, nstep, style=3)
    for(n in 1:nstep)
    {
  #    setTxtProgressBar(pb, n)
      stores$ex <- 0
      stores.in <- current.storage(groups, stores, ichan) 
      updated <- update.subsurface(groups, flows, stores, weights,                                  
                                 pe=pe0, 
                                 tm, 
                                 ntt, dt, ichan)
      #qins <- rbind(qins, updated$qin.riv)
      # net recharge: drainage minus discharge to river
      recharge.t <- dt*sum(groups$area*flows$uz) - sum(updated$Qriv)
      recharge <- c(recharge, recharge.t)
      # now includes any overland storage due to saturated excess
      
      storage.gain.t <-current.storage(groups, stores, ichan)-stores.in
      storage.gain <- c(storage.gain, 
                        storage.gain.t) 
      bal <- c(bal, recharge.t-storage.gain.t)  # +ve indicates more water in than storage increase
      flows <- updated$flows
      stores <- updated$stores
      qbfs <- rbind(qbfs, flows[-ichan,]$qbf)
      qriv <- c(qriv, updated$Qriv[i.outlet]*groups[i.outlet,]$area)
      deficits <- rbind(deficits, stores[-ichan,]$sd)
      #   cat(tm, "\t", flows$qbf, "\n")
      
      # tm<-c(tm, dt*3600)
    }
  #  close(pb)
    bals <- cbind(bals, bal)
    qrivs <- cbind(qrivs, qriv)    
  }

  stores$ex <-0
  return(list("flows"=flows, "stores"=stores))
}
