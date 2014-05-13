as.SpatialLines1 <-
function(line, crs=CRS(NA), ID="id")
{
	if(CheckClass(line, "SpatialLines")){return(line)} # nothing to do
	if(is.matrix(line))
	{
		if(ncol(line)!=2 | nrow(line)<2){stop("invalid line coordinates matrix")}
		line <- Line(line)
		
	}
	if(CheckClass(line,"Line"))
	{
		line <- Lines(list(line), ID=ID)

	}
	if(CheckClass(line,"Lines"))
	{
		return(SpatialLines(list(line), crs))		
	}
}
