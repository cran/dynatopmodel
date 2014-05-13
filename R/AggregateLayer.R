AggregateLayer <-
function(rast, fact=1)
{
	if(!is.null(rast) & fact>1)
	{
		cat("Aggregating raster layer by factor of ", fact, "... ")	
		
		(rast <- aggregate(rast, fact))
		
	}
}
