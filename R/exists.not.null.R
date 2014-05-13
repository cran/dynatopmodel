exists.not.null <-
function(obj.name, check.file=T, warn=NULL)
{
  res <- FALSE
  # calling frame / environment, up one level in calling stack, must be at least one
  p.env <- sys.frame(-1)
  # treat the argument as a charcater object name and look in the calling frame
  if(exists(obj.name, where=p.env))
  {
    obj <- get(obj.name, p.env)
    if(!is.null(obj))
    {
      res <- ifelse(check.file,
                    file.exists(obj),
                    TRUE)
      
    }
  }
  if(!res & !is.null(warn))
  {		
    warning(warn)				
  }
  return(res)
}
