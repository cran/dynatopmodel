WeightedRandomSelection <-
function(p,n=1)
{
	# divide unit interval up in proportion to weights
	q<-cumsum(p)
	
	if(is.na(sum(p)))
	{
		browser()
	}
	
	# random slection from uniform dist in [0,1] 
	r<-runif(n)
	
	# in which intervals are the samples located (start at 0, add 1)
	return(findInterval(r,q)+1)
	
}
