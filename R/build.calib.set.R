build.calib.set <-
function(groups, params)
{
	par.sets <- expand.grid(params)
	nms <- c("vchan", "vof", "td", "srz0", "m", "ln_t0", "srz_max")
	# create a table bifg enough to hold the permuations of parameters
	gr <- as.numeric(as.matrix(groups[2,nms]))
	 
	# params must be named
	n.set <- nrow(par.sets)
	
	calib.set <- data.frame(matrix(rep(gr, n.set), nrow=n.set, byrow=T))
	colnames(calib.set)<- nms
	calib.set[,colnames(par.sets)] <- par.sets
	
	return(calib.set)
}
