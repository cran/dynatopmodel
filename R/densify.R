densify <-
function(xy,n=5){
	## densify a 2-col matrix
	cbind(dens(xy[,1],n=n),dens(xy[,2],n=n))
}
