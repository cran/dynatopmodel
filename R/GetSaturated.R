GetSaturated <-
function(stores, ichan)
{	
	stores[ichan,]<-0.1
  sat <- which(stores$sd<=0)
  return(sat)
}
