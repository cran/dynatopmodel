MergeLinesList <-
function(lines, ID="")
{
	# if SpatialLines create a list split into 
	lines<-lapply(1:length(lines), function(il){lines[il,]})
	#	if(!is.projected(from)){stop("Projected coordinates system required")}
	crs <- projection(lines[[1]])
		
	
	ll <- lapply(lines,
		   function(l)
		   {
		   		Line(GetCoords(l))
		   })
	if(ID=="")
	{
		ID <- lines[[1]]@lines[[1]]@ID}
	
	SpatialLines(list(Lines(ll, ID=ID)), proj4string=CRS(crs))
	#coords <- do.apply(rbind, coords)
#	SpatialLines(list(Lines(list(Line(coords)), ID=ID)), proj4string=crs)	
	
}
