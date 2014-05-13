load.obs <-
function(proj,  obs.dir=proj$obs$dir)
{
  if(!is.null(obs.dir) & file.exists(obs.dir))
  {
    proj$obs$dir <- obs.dir
    rain <- NULL
    pe <- NULL
    qobs <- NULL    
    qobs.fn <- dir(obs.dir, "qobs*.*$")
    rain.fn <- dir(obs.dir, "rain*.*$")
    pe.fn <- dir(obs.dir, "pe*.*$")
    # specfying large number  - will be lmited to number present. load data for all locations
    try(rain <- read.obs(fp(obs.dir, rain.fn[1]), val=2:10, sep="\t"), silent=F)
    try(qobs <- read.obs(fp(obs.dir, qobs.fn[1]), val=2:10, sep="\t"), silent=T)
    try(pe <- read.obs(fp(obs.dir, pe.fn[1]), val=2:10), silent=T)
    proj$obs$rain <- rain
    proj$obs$qobs <- qobs
    proj$obs$pe <- pe
  #  try(proj$qmax <- max(qobs, na.rm=T), silent=T)    
#    try(proj <- fix.run.dates(proj))  
  }
  return(proj)
}
