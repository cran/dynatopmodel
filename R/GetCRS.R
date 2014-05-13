GetCRS <-
function(obj)
{
  if(projection(obj)=="NA")  
  {
    return(CRS(NA))
  }
  return(CRS(projection(obj)))
}
