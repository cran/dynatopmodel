normalise <-
function(x)
{
	if(is.vector(x))
	{
		tot <- sum(abs(x), na.rm=T)
		
		return(ifelse(tot==0,x,x/tot)) 
	}
	return(x)
}
