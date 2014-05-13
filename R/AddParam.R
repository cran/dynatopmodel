AddParam <-
function(groups, name, val)
{
  if(!name %in% names(groups))
  {
    # add column with default values if not found
    groups <- cbind(groups, list(val))
    names(groups)[length(groups)]<-name
    return(groups) 
  }
  
  # just return the input
  return(groups)
  
}
