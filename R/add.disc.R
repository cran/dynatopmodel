add.disc <-
function(proj, cuts=NULL, i.disc=0, rebuild=F, chan.width=2,										
										 ...)
{
 # proj <- merge.lists(proj, list(...))
  
	# build names from extra parameters
	cuts <- merge.lists(cuts, list(...))
	
  # name wil be infered from cuts
  dn <- disc.dir.name(cuts, dn=proj$disc.dir)
  
 # rebuild <- file.exists(dn)
  if(rebuild){message(paste("Rebuilding files in ", dn))}
  disc <- NULL
  try(disc <-disc.from.dir(dem=proj$dem, 
                           drn=proj$drn, 
                           reaches=proj$reaches, 
  												 routing=proj$routing, 
  												 catch=proj$catch,
                           dn=dn,   
                           cuts=cuts,
                           rebuild=rebuild,
                           chan.width=chan.width,
                           area.thresh=proj$area.thresh), silent=F)  

  if(!is.null(disc))
  {
    if(is.null(i.disc) | i.disc <=0 | i.disc > length(proj$disc))
    {
      return(disc)
    }
    # same cuts as any other?
    proj$disc[[i.disc]]<- disc   
    return(proj)
  }
  else
  {
    warning("Adding discretisation ", paste(cuts, collapse=","), "failed")
  }
}
