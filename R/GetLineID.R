GetLineID <-
function(l, n=1)
{
	classname <- class(l)[1]
	obj <- l
	
	if(classname == "SpatialPolygons"){
		obj <- l@polygons[[n]]
	}
	else if(classname == "SpatialLines"){
		obj <- l@lines[[n]] 	
		#return(as.numeric(l@lines[[1]]@ID))	
	}
	else if(classname == "Lines"){
		#return(as.numeric(l@lines[[1]]@ID))	
	}
	else
	{
		stop(paste("Invalid class ", classname, " passed to GetLineByID"))	
		#return(as.numeric(l@lines[[1]]@ID))	
	}
	
	return(obj@ID)
}
