FixNullOrLt <-
function(x, min=0)
{
	if(NullOrLt(x, min))
	{
		return(min)
	}
	return(x)	
}
