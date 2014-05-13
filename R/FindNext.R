FindNext <-
function(x)
{
 # browser()
  cur <- x[5]
  if(is.na(cur))
  {
    return(NA)
  }
  # weight straight directions more heavily
  dz <- x[5]-x
  return(which.max(dz))
}
