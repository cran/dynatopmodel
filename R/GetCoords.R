GetCoords <-
function(obj, collate=F)
{
	if(is.null(obj)){stop("Cannot get null coordinates")}
	if(is.matrix(obj) | is.vector(obj))
	{
		obj <- matrix(obj, ncol=2)
		if(ncol(obj)==2)
		{
			return(obj)
			
		}
		else
		{
			stop("Invalid coordinates matrix supplied, 2 columms required ")
		}
	}
	# see if this object has a "lines" attribute, if so, check the size of its
	# first Lines attribute
	if(inherits(obj, "SpatialLines") & is.list(obj@lines))
	{
    
      coord.list<-lapply(obj@lines,    
        function(ls)
        {
      
  	      # recurse into list (need to apply to top level too)
          lapply(ls@Lines, 
                         function(l)
                         {                           
                           l@coords
                          # coordinates(l)
                         }
                         )
        }    
      )
    
    if(collate)
    {
		  return(do.call(rbind, coord.list))
    }
    else
    {
      return(coord.list)      
    }
	}
	return(matrix(unlist(coordinates(obj)), ncol=2)	)
}
