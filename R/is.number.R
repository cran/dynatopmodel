is.number <-
function(s)
{
  w <- as.numeric(options("warn"))
  options("warn"=-1)
  val <- F
  try(val <- !is.na(as.numeric(s)), silent=T)
  options("warn"=w)
  return(val)
  
}
