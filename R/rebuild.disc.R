rebuild.disc <-
function(proj, i.disc=1, disc=NULL, dn=disc$dir, 
                         what="*.dat|*.tif",...)   # specify what is to be rebuilt
{
  if(is.null(disc)){disc <-proj$disc[[i.disc]]}
  # add in any new or updated data
  args <- list(...)
  disc$cuts <- merge.lists(disc$cuts, args)
  if(length(disc$cuts)==0)
  {
    stop("Error, can't locate any cuts for given discretisation. Please specify, for example: projdisc[[1]]$cuts=c(a=10)")
  }

  try(write.disc(disc), silent=T)
  dem <- proj$dem
  drn <- proj$drn
  # remove specified files
  lapply(dir(dn, what, full.names=T),				 				 
         file.remove)

  
  disc <- disc.from.dir(disc$dir, dem=dem, drn=drn, 
                        area.thresh=disc$area.thresh,
                        chan.width=disc$chan.width, 
                        cuts=disc$cuts,rebuild=T)
  
  return(disc)
}
