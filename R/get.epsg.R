get.epsg <-
function(nm)
{
  switch(nm,
         
         # EPSG strings
         latlong = "+init=epsg:4326",
         lonlat = "+init=epsg:4326",
         ukgrid = "+init=epsg:27700",
         google = "+init=epsg:3857"
  )
}
