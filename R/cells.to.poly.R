cells.to.poly <-
function(dem,cells)
{
	cells <- unique(cells)
	polys <- lapply(cells,
		function(cell)
		{
			ext <- CellExtent(dem, cell)
			PolygonFromExtent(ext, crs=dem@crs, id=cell)
		}
	)
	poly <- do.call(rbind,polys)
	# list of polys
	res<-gUnionCascaded(poly)
	return(res)
}
