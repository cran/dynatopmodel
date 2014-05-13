current.input <-
function(groups, rain, ae, qr)
{
  return(as.numeric(rain - WeightedAverage(ae, groups$area) - qr))
}
