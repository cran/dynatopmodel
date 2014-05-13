PlotGroupings <-
function(cm, dem, drn, sel=extent(cm),legend=T, gridcells=F, ...)
{

	groups <- build.hru.table(cm)
	ngroups <- nrow(groups)
	nchan <- length(which(groups[,1]<100))
	cm <- cm[[1]]	
	# substitute values so group numbers are sequenetial
	cm <- subs(cm, groups)
	cm<-cm-nchan
	cm[which(cm[]<0)]<-0
	cols <-  terrain.colors(ngroups-nchan)
	#(rep("blue", nchan),
	PlotDemADrn(dem, drn, a=cm, sel=sel, col=c("lightblue", cols), legend=F, ...)
	if(legend)
	{
	legend(x="bottomleft", legend=groups[(nchan+1):nrow(groups),3], 
		   ncol=2,bg="white",fill=cols, title="Eff distance to channel (m) / log(a) / slope angle ?")
	}
	
	if(gridcells)
	{
		HighlightCells(cm,cellsFromExtent(cm,sel))
	}
	
}
