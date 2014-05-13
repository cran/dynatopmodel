route.ovf <-
function(groups, flows, stores, w, dt, ichan=1, nstep=10, debug=F)
{
	ex <- stores$ex
	if(any(ex>0) )
	{
		dtt <- dt/nstep
		#		ex[ichan]<- 0 # 
		#	if(sum(ex[ichan])>0) {stop("excess channel storage!")}
		w.in <- ex*groups$area
		# channel flux remains there until return		
		groups[ichan,]$vof <- 0
		ex.dtt <- ex
		flows.dtt <- flows
		for(i in 1:nstep)
		{ 
			# total flow out of all groups, limited to storage available
			flows$qbf <- pmin(ex*groups$vof, ex/dtt)
			
			# total flow into each group from elsewhere (including recycled flux)
			# channel recyles flux into itself (indicated by NULL ichan )
			flows$qin <- dist.flux(groups, flows, w, ichan=ichan)
			
			flows.dtt <- rbind(flows.dtt, flows)
			
			# difference in specific flux over time step
			ex <- ex + dtt*( flows$qin/groups$area- flows$qbf)
			# ex reduced below zero: 
			ex.dtt <- rbind(ex.dtt, ex) 
		}
		
		# convert to a specific storage
		#	ex <- ex/groups$area
		# water balance check
		w.out <- ex*groups$area
		bal <- 100*sum(w.out-w.in)/sum(w.in)
		if(abs(bal) > 1)
		{		
			browser()
			warning("Water balance check error")
		}
	}
	
	return(ex)  
}
