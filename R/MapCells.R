MapCells <-
function(src, targ, 
					 cells=1:ncell(src)) # cell indeex
{
	fact <- round(sqrt(ncell(targ)/ncell(src))) # this assummes src is larger than targ, i.e. has been aggregated from it
	
	#sapply(cells, MARGIN=1, FUN=function(cell){MapCell(src,targ,cell)})
	res<-NULL
	for(cell in cells)
	{
	#		cat(cell, "\n")
			ccells <- MapCell(src,targ,cell)
		 	res <- c(res,ccells)
	}
	
	return(res)
}
