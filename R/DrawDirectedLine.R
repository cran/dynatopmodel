DrawDirectedLine <-
function(line, 
							 col="slategray", 
							 lwd=1,
							 len=0.04)
{ 
	x <- line@coords[,1]
	y <- line@coords[,2]
	DrawDirectedLineSegments(x,y,col,lwd,len)	
}
