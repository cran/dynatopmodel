MatchClass <-
function(obj, names) #c("SpatialLines", "SpatialLinesDataFrame"))
{
  return(class(obj)[[1]] %in% names)
  
 # return(class(obj)[2] %in% names)
  
}
