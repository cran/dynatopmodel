rast.mem.copy <-
function(rast)
{
  if(raster::inMemory(rast)){return(rast)}
#  rast.copy <- stack(rast)
  layers <-
    lapply(names(rast), 
         function(ln)
         {
            layer <- rast[[ln]]
            raster::setValues(layer, getValues(layer))
         }
  )
  if(length(layers)>1)
  {
    return(stack(layers))
  }
  else
  {
    return(layers[[1]])
  }
}
