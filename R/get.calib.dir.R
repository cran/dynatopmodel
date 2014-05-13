get.calib.dir <-
function(proj.ex)
{
	s <- format(proj.ex$run.par$start, "s=%Y-%m-%d")
	e <- format(proj.ex$run.par$end, "e=%Y-%m-%d")
	return(paste0(s, e, collapse=","))	
}
