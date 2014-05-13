CellBoundingPoly <-
function(dem, cells, width=xres(dem)-1)
{
	pts <- SpatialPoints(xyFromCell(dem,cells), dem@crs)	
	src <- gBuffer(pts,
				   width=width, byid=T)
	
	# return one large polygon
	return(gUnionCascaded(src))
}
