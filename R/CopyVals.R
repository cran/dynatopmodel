CopyVals <-
function(from, to,  
         all=F)  #
{
  is.list <- F
  if(is.list(to))
  {
    is.list <- T
    # convert to environment for copying, convert back on return
    to <- list2env(to)
  }
   
  nms.to <- ls(to)
  for(nm in ls(from, all.names=TRUE))
  {
    if(nm %in% nms.to)
    {
      # assign name-value in this envir to that specified
      assign(nm, get(nm, from), pos=to)
    }
  }
  if(is.list)
  {
    to <- as.list.environment(to)
  }
  return(to)
    
}
