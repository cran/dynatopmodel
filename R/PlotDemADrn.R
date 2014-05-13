PlotDemADrn <-
function(dem, drn, a=NULL, sel=extent(dem), 
						grid=F, grid.freq=3,
						grid.col="gray",
						compass = F,
						contour=T,
						nlevels=10,
						compass.pos = "topleft",
						scalebar=F, scalebar.dist=500, ...)
{
	dem <- crop(dem,sel)
	if(!is.null(a))
 	{	
		a <- crop(a,sel)
		plot(a,...)
	}
	else
	{
		plot(dem, ...)
	}
	if(grid)
	{
	#	dem <- aggregate(dem,grid.freq)		
	#	cells <- which(!is.na(getValues(dem)))
		
	#	HighlightCells(dem,cells,col=grid.col)
		grid(col=grid.col)
	}
	if(scalebar)
	{
	#	labs<-pretty(0:scalebar.dist,n=5)		
		scalebar(d=scalebar.dist,below="m",
			#	 label=labs,
				 divs=4,
				 type="bar")
	}
#	drnexp2<-gIntersection(drnexp, PolygonFromExtent(sel, asSpatial=T, dem@crs))
	plot(drn,col="blue",add=T,lwd=2)
	if(contour)
	{
	contour(dem,add=T,nlevels=nlevels)
	}
	if(compass)
	{
		compassRose(x=compass.pos, cex=0.75)
	}
}
