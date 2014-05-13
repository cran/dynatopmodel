CheckClassAndNotNull <-
function(x, expected, name)
{
  if(is.null(x)){
    stop(paste("Unexpected NULL input for parameter ", name))
  }
  else if(class(x)[[1]] != expected)
  {
    stop(paste("Unexpected class for input for parameter, ", name,  ": expected ", expected, ", found ", class(x)[[1]]))
  }
  return(TRUE)
}
