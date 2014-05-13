StoresEmpty <-
function(groups, stores)
{
  return(which(stores$srz<=0 & groups$SRmax>0))
}
