BufferLines <-
function(lines, width=2)
{
	lbuffs<- lapply(lines,
		function(l)
		{
			id <- GetLineID(l)
			lbuff <- gBuffer(l, width)	
			lbuff@polygons[[1]]@ID <- as.character(id)
			return(lbuff)
		}
	)
	
	return(lbuffs)
}
