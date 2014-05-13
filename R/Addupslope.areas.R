Addupslope.areas <-
function(groups, dem, class.m, 
														area_pcs=round(dem/dem))   # optional ratser of cell areas occupied by land
{
  if(!(is.null(dem) | is.null(class.m)))
  {
    # mean ln(a/tan(b))
    a.atb <- upslope.area(dem, atb=T)
    # deal with cells partly ocuppied by river channel
    atb.adj <- a.atb[["atb"]]*area_pcs
    # zonal statistics - mean is default. Values adjusted for any cells containing 
    # channel
    atb <- zonal(atb.adj, class.m, "mean")  # deals with nas
        
    # specific discharge per unit recharge [-] assuming steady state.
    # a measure of the relative yield of the area?
  #  sigma.a <- zonal(raster::setValues(dem,a.atb$area), cm, "mean")  # this must have the same first row    
    for(row in 1:nrow(atb))
    {
      id <- atb[row,1] 
      indx <- which(groups$"id"==id)
      if(length(indx)==0)
      { 
        warning(paste("index ", id, "from class matrix not found in groups table"))
      }
      else
      {
      #  cat(id, "\t", indx, "\n")
      # this assummes that every id in the raster has a corresponding id in the table....
      groups[indx,"atb.bar"]<- atb[row,2]  
      # q over r
     # groups[indx,"sigma.a"]<- sigma.a[row,2]/groups[indx,]$area
      }
    }   
  }
  return(groups)
}
