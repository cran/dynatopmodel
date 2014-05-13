HighlightCell <-
function(dem, cell, col=NA, label=F, border=par("fg"),...)
{
	ext <- CellExtent(dem,cell)	
#	plot(ext,add=add,...)
#	if(!is.na(fillcol))
#	{
		rect(xleft=ext@xmin, xright=ext@xmax, ybottom=ext@ymin, ytop=ext@ymax, 
         col=col, border=border, ...)
#	}
  # label with cell number, centred
  if(label)
  {
    xbar <- (ext@xmin+ext@xmax)/2
    ybar <- (ext@ymin+ext@ymax)/2
    text(labels=cell, x=xbar, y=ybar,...)
  }
  
#	rect(xleft=ext@xmin, xright=ext@xmax, ybottom=ext@ymin, ytop=ext@ymax,...)	
}
