extract.cells <-
function(dem, drn)
{
	target <- extract(dem, drn, cellnumbers=T)
	if(is.list(target))
	{
		return(do.call(rbind, target)[,1])
	}
	else
	{
		return(target[,1])
	}
}
