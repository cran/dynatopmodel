disc.dir.name <-
function(cuts, area.thresh=0, dn="")
{
  # convert to %age
#  if(area.thresh<1){area.thresh <- area.thresh*100}
  return(fp(dn, paste0(names(cuts),"=", cuts, collapse=",")))
  
}
