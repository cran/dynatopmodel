ResetStorages <-
function(stores, ichan)
{
  # tidy up

  # remove overland flows - also allocated in RouteFlow routine
  stores[, c("ex")]<- 0
  return(stores)
}
