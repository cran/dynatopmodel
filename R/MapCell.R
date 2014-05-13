MapCell <-
function(src, targ, cell)
{
	fact <- round(sqrt(ncell(targ)/ncell(src))) # this assummes src is larger than targ, i.e. has been aggregated from it
	rc<- raster::rowColFromCell(src, cell)
	# expand to cell row /col number indexes 
	rownr <- ((rc[1]-1)*fact+1):(rc[1]*fact)
	colnr <- ((rc[2]-1)*fact+1):(rc[2]*fact)
	return(cellFromRowColCombine(targ, rownr, colnr))
}
