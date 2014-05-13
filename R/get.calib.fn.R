get.calib.fn <-
function(proj.ex, par.min.max)
{
  ngroup <- nrow(proj.ex$groups)
  par.nm <- paste0(names(par.min.max), "=", par.min.max, collapse=",")
  tm <- format(Sys.time(), "%Y-%m-%d %H.%M.%S")
  s <- format(proj.ex$run.par$start, "s=%Y-%m-%d")
  e <- format(proj.ex$run.par$end, "e=%Y-%m-%d")
  
  return(paste0(s, ",", e, "/", par.nm, ",", tm, ".dat"))
  
}
