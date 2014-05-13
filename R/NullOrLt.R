NullOrLt <-
function(x, min=0)
{
	if(is.null(x))
	{
		return(TRUE)
	}
	if(is.numeric(x))
	{
		return(x<min)
	}
	warning("Non-numeric argument supplied")
	return(TRUE)	
}
