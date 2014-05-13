CheckNotNull <-
function(obj, stop=FALSE)
{
  if(!exists(obj)|is.null(obj))
  {
    if(stop)
    {
      stop(paste("value for ", obj, " required"))
    }
    return(FALSE)
  }
  return(TRUE)
}
