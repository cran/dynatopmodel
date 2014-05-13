statmod <-
function(x)
{
	z <- table(as.vector(x))
	if(length(x[!is.na(x)])==0)
	{
		# nothing non-NA so return this
		return(NA)
	}
	else
	{
		return(names(z)[z == max(z)])
	}
}
