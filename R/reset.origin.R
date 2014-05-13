reset.origin <-
function(rast, origin=c(x=0,y=0))
{
  shift(rast, origin[1]-extent(rast)@xmin, origin[2]-extent(rast)@ymin)
  
}
