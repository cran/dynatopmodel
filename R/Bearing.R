Bearing <-
function(src, dest, nthvect=c(0,1), 
					quadrant=F)
{
	# convert to coordinate frame wrt north in x direction and e in y direction
	# project the src -> line onto E arrow to get x coord
	# normal
	y <- nthvect
	x <- c(y[2], -y[1])
	# direction vector, transform to coordinate wr.t x and y (N and E)
	r <- dest - src
	# projection onto x axis
	rdashx <- sum(r*x)
	# projection onto y axis
	rdashy <- sum(r*y)
	
	if(quadrant)
	{
		# return text indicating compass quadrant of direction vector
		dir <- NULL
		# return N, W, E, S
		if(rdashy>0){
			dir<-"N"
		}
		else if(rdashy<0){
			dir<-"S"
		}
		if(rdashx>0){
			dir <- c(dir,"E")
		}
		else if(rdashx<0){
			dir <- c(dir,"W")
		}	
		return(dir)
	}
	else
	{
		tanTheta <- rdashx/rdashy
		return(atan(tanTheta)*180/pi)
	}
}
