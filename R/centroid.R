centroid <-
function(ext)
{
	CheckClass(ext, "Extent", stop=T)
	
	x<-mean(c(ext@xmin, ext@xmax))
	y<-mean(c(ext@ymin, ext@ymax))
	return(cbind(x,y))
}
