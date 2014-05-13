do.build.flow.matrix <-
function(dem, hru, drn, chan.width=4, fact=2)
{
	message("aggregating....")
	dem <- aggregate(dem, fact)
	hru <- aggregate(hru, fact, fun=modal)
	message("getting reaches")
	reaches <- determine.reaches(dem, drn, chan.width=chan.width)
	message("building...")
	w <- build.flow.dist.matrix(dem, hru, reaches=reaches)	
}
