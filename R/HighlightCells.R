HighlightCells <-
function(dem,cells, col=NA, label=F, 
						   border=par("fg"), ...)
{
	if(length(cells)>0)
	{
		# produce a colur vector of same length as the vector of cells to highlight
		fillcols <- rep(col, length.out=length(cells))
		bcols <- rep(border, length.out=length(cells))		
		for(cellno in 1:length(cells))
		{
			cell<-cells[cellno]
			col <- fillcols[cellno]
			bcol <- bcols[cellno]
			HighlightCell(dem,cell,
						  col=col, label=label,
						  border=bcol,...)
					
		}
	}

}
