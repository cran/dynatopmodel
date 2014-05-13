UniqueExcludingNAs <-
function(x,fromLast=F)
{
	x <- unique(x, fromLast=fromLast)
	return(x[!is.na(x)])
}
