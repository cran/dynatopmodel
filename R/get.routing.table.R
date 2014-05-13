get.routing.table <-
function(dem, nreach=5)
{
  dem <- raster::setValues(dem, topmodel::sinkfill(as.matrix(dem), degree=0.1, res=10))
  
  fl <- raster::setValues(dem, topmodel::flowlength(as.matrix(dem)))*xres(dem)
  
  routing.hist <- hist(fl, breaks=nreach)
  n <- sum(routing.hist$counts)
  # build the table@ first column is disatnce to outlet, second the cumlative area
  routing <- cbind(prop=cumsum(routing.hist$counts)/n, d=routing.hist$mids)
  return(routing)
}
