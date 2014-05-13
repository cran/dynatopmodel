Last <-
function(x)
{
	if(length(x)>0)
	{
		return(x[length(x)])
	}
	warning("Attempted to find last element of zero-length vector")
	return(NULL)	
}
