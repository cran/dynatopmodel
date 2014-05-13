current.storage <-
function(groups, stores, ichan=NULL)
{
  if(!is.null(ichan))
  {
    stores <- stores[-ichan,]
    groups <- groups[-ichan,]

  }
  
  # storage remaining in root and unsaturated zones plus saturated storage in water table
  # subtracting the sd gives the effective overall storage across all zone. -ve values indicate deficit
  
  return(WeightedAverage(stores$srz+stores$ex+stores$suz-stores$sd, groups$area))  
}
