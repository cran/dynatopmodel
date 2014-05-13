StoresFull <-
function(groups,stores)
{
  return(which(stores$srz>=groups$srz_max)) #  & stores$srz>0))
}
