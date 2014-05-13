CellExtent <-
function(dem, cell)
{
	ptmid <-xyFromCell(dem,cell, spatial=T)
	# extend to edge of cell
	xymin<- ptmid@coords - c(xres(dem),yres(dem))/2
	xymax<- ptmid@coords + c(xres(dem),yres(dem))/2
	ext <- extent(matrix(cbind(xymin,xymax),ncol=2))
	return(ext)
}
