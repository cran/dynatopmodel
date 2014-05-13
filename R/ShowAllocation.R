ShowAllocation <-
function(dem, cellno, maxwidth=5, cex.text=1, label=T)
{
	down <- DownslopeFlowAllocations(dem, cellno)
	
	fromxy <- xyFromCell(dem, cellno)

	apply(down, MARGIN=1,
		  FUN=function(tocell)
		  {
		  	width <- max(1,tocell[2]*maxwidth) # < maxwidth
		  	toxy <- xyFromCell(dem, tocell[1])
		  	arrows(fromxy[1],fromxy[2],toxy[1],toxy[2],lwd=width,length=0.1)
		  	if(label)
		  	{
		  	mid <- (fromxy+toxy)/2
		  	text(mid[1], mid[2], round(tocell[2],1), cex=cex.text)}
		  } 
	)
}
