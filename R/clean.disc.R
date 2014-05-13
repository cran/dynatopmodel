clean.disc <-
function(disc)
{
  disc <- lapply(disc, function(disc){if(is.list(disc)){return(disc)}})  
  disc <- delete.NULLs(disc)
  disc <- lapply(disc, function(disc){if(!is.null(disc$groups)){return(disc)}})  
  disc <- delete.NULLs(disc)
  disc <- sort.disc(disc)
  return(disc)
}
