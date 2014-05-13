fix.run.dates <-
function(proj)
{
  obs <- proj$obs$rain
  
  if(!is.null(obs) & !check.time.intervals(proj))
  {
    
    warning("No rainfall data within specified run start/ end times: adjusting...")
    len.sim <- intervals::size(intervals::Intervals(range(proj$sim.start, proj$sim.end)))
    
    start <- start(index(obs$rain))
    cat("Setting sim start to ", "\n")
    proj$sim.start <- start
    end <- min(start + len.sim, end(index(obs$rain)))
    cat("Setting sim end to", end, "\n")
    proj$sim.end <- as.POSIXct(end)    
  } 
  return(proj)
}
