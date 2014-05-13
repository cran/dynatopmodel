CheckClass <-
function(x, req, stop=FALSE)
{
  if(!is.null(x))
  {
  	# case-insensitive check
    res <- toupper(class(x)[[1]]) == toupper(req)
    if(!res & stop)
    {
      stop(paste("Object is of type ", class(x)[[1]], ", but expected type is ", req))
    }
    return(res)
  }
  return(TRUE)
}
