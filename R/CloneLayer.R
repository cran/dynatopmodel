CloneLayer <-
function(r, n, names=NULL)
{
	b<-brick(lrep(r, n))
	names(b)<- names
	return(b)
}
