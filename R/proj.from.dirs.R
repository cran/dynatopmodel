proj.from.dirs <-
function(disc.dir, 
                           root.dir=NULL, donor=NULL, obs.dir=NULL)
{
  
  proj <- create.proj(fn=NULL, data.dir = disc.dir, root.dir=root.dir)
  
  if(!is.null(donor))
  {
    #proj$groups <- donor
    
    proj$groups[,par.names] <-  donor$groups[nrow(donor$groups),par.names] #), nrow(proj$groups))
    proj$run.par <- donor$run.par
    proj$disp.par <- donor$disp.par
    proj$dt <- donor$dt
    proj$ntt <- donor$ntt
    proj$rain <- donor$rain
    proj$qt0 <- donor$qt0
    proj$qmax <- donor$qmax
  }
  # look for data 
  return(proj)
}
