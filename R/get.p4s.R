get.p4s <-
function(nm)
{
	# note discrepancy with osgb <=> wgs84 conversion with proj4 strig pre Oct 2007
	# http://stackoverflow.com/questions/1426941/proj-4-library-and-osgb36
  switch(nm,
  			 osgb = "+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.9996012717 +x_0=400000 +y_0=-100000 +ellps=airy +towgs84=446.448,-125.157,542.060,0.1502,0.2470,0.8421,-20.4894 +units=m +no_defs",
    #     osgb ="+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.9996012717 +x_0=400000 +y_0=-100000 +ellps=airy +towgs84=446.448,-125.157,542.06,0.15,0.247,0.842,-20.489 +units=m +no_defs",
         # spherical mercator,
         merc ="+proj=get.p4s(merc) +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null +no_defs",
    		# wgs84
    		wgs84 = "+proj=longlat +ellps=WGS84 +towgs84=0,0,0 +no_defs"
    
  )
}
