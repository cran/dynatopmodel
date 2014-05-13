dist.flux <-
function(groups, flows, 
											W,  # flow distribution matrix 
											#dtt, # time step, inner or outer (needed?)
											ichan=1)  # channel identifiers
{  
	# total baseflow
	QBF <- flows$qbf*groups$area  # total base flow
	if(length(ichan)>0)
	{
		# ensure river fluxes doesn't get recycled to itself (why?)
		diag.chan <- matrix(rep(ichan, 2), ncol=2)
		
		W[diag.chan]<- rep(0, length(ichan)) 
	}
	
	# all land - land, land-riv and riv - riv transfers
	# distribute base flux to inputs of other HSUs
	#flows$qin <- 
	return(as.vector(QBF %*% W))
	
	# qin is *total* input flow into areal groups
	#  return(flows$)   					   	
}
