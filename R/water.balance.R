water.balance <-
function(groups, stores, dt, storage.in, ichan, 
                          qsim,                        
                          rain, ae=0)
{
  # rain distribution
  areas <- matrix(rep(groups$area, nrow(rain)), ncol=nrow(groups), byrow=T)
  tot.rain <- rowSums((rain[,groups$gauge.id]-ae)*areas)/sum(groups$area)
  tot.rain<-tot.rain[1:nrow(qsim)]
  # difference between net input and net storage gain
  storage.gain <- current.storage(groups, stores, ichan)-storage.in
  wb <- dt*sum(tot.rain - qsim[,1], na.rm=T)-storage.gain
  return(wb)
}
