convert.to.specific.discharges <-
function(proj, q)
{
	if(max(q))
	# catchment area
  a <- with(proj, sum(length(which(!is.na(dem[])))*xres(dem)*yres(dem)))
  # assumme in cu.m/s
  res <- 3600*q/a
  if(max(res, na.rm=T)>1){warning("Very large specific discharges calculated: check input not in mm")}
  
  return(res)
}
