SlopeGDAL <-
function(dem.fn, out.fn=NULL)
{
  if(is.null(out.fn))
  {
    out.fn <- "slope.tif"
  }
  dir <- Sys.getenv("GDAL_DIR")
  if(is.null(dir))
  {
    stop("GDAL directory not set: use Sys.setenv('GDAL_DIR')")
  }
  exe  <- file.path(dir, "gdaldem.exe")
  shell(paste(exe, "slope", dem.fn, out.fn))
  return(raster(out.fn))
}
