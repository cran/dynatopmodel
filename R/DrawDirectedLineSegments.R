DrawDirectedLineSegments <-
function(vertX, vertY, 
									 col="slategray", 
									 lwd=1,
									 len=0.04) # small arrows
{
	if(length(vertX) != length(vertY)){stop("Mismatched lengths for arrow vertices coords")}
	# destinations (heads) for arrows
	x1 <- vertX[-1]
	y1 <- vertY[-1]
	
	# sources - note one less
	x0 <- vertX[1:length(x1)]
	y0 <- vertY[1:length(y1)]
	
	if(length(x1)>0 & length(x0)>0)
	{	
		arrows(x0,y0,x1,y1,length=len, lwd=lwd, col=col)	
	}
}
