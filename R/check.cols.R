check.cols <-
function(obj, names, stop=T)
{
  msg <- NULL
  for(nm in names)
  {
    if(!(nm %in% colnames(obj)))
    {
      msg <- paste(msg, "Missing column name ", nm, "\n")
    }
  }
  if(length(msg)>0 & stop){stop(msg)}
  return(msg)
}
