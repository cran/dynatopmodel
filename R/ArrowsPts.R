ArrowsPts <-
function(frompt,topt, ...)
{
	c0 <- GetCoords(frompt)
	c1 <- GetCoords(topt)
	
	arrows(x0=c0[1], y0=c0[2], x1=c1[1], y1=c1[2], ...)
	
}
