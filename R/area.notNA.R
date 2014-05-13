area.notNA <-
function(rast)
{
  if(is.null(rast@crs) | !is.projected(rast))
  {
    warning("Projected coordinate system required when calculating area. Raster uses geographical system")
  }
  return(xres(rast)*yres(rast)*length(which(!is.na(rast[]))))
  
}
