disc.from.dir <-
function(dn,  dem,  reaches=NULL, 
													routing=NULL, # optionally supply routing table
													cuts=NULL, # cuts and threshold applied if no directory 
                          area.thresh=NULL, 
                          drn=NULL, 
                          chan.width=NULL, 
                          rebuild=F, dn.out=dn, 
													catch=NULL,  # an input stack
													...)   # additional parameters wil be treated as cuts
{  
	# build names from extra parameters
	cuts <- merge.lists(cuts, list(...))
	
  if(is.null(cuts))
  {  	
    # if not supplied load cuts and other discretisation info from this directory
    par.fn <- dir(dn, "*.par$", full.names=T)
    
    for(fn in par.fn)
    {
      cat("Trying settings from ", fn, "...\n")
      try(source(fn, local=TRUE, echo=F), silent=F)
    } 
    #if(is.null(cuts)){return(NULL)}
  }  
	if(is.null(area.thresh)){
	  area.thresh<- get.defs()$area.thresh
	}	
	if(is.null(chan.width)){
	  chan.width<- get.defs()$chan.width}	
	if(!file.exists(dn)){ 	  
	  dir.create(dn, recursive=T)
	}  
  
	cat("Loading discretiation from ", dn, "...")

	hru.sp <- NULL
	hru <- NULL
  fn <- file.path(dn, "hru.tif")
	try(hru <- raster::raster(fn), silent=T)
	
	# convert to proportion
  if(area.thresh>=0.1){area.thresh<-area.thresh/100}
  
  try(drn  <- rgdal::readOGR(dn , "drn"),silent=T)

  chan.props <- NULL
  fn <- file.path(dn, "reaches.tif")
  if(file.exists(fn)){try(reaches <- stack(fn), silent=T)}
	
	# shape file, if it exists
 # try(hrus <- rgdal::readOGR())
  # every raster in dir contributes to catch discretisation
  catch.fn <- setdiff(dir(dn, "*.tif"), c("hru.tif", "reaches.tif"))
  catch.layers <- lapply(catch.fn, function(fn)
      {
  			layer<-NULL
        try(layer <- stack(file.path(dn, fn)), silent=T)
        try(layer <- crop(layer, extent(dem)), silent=T)
      })

  # will fail if nothing supplied or found 
  try(catch <- stack(c(catch, delete.NULLs(catch.layers))), silent=T)
	
	disc.dir <- dn 
	weights <- load.disc.data(file.path(disc.dir, "weights.dat"), rebuild=rebuild)
	groups <- load.disc.data(file.path(disc.dir, "groups.dat"), rebuild=rebuild)
  routing <- load.disc.data(file.path(disc.dir, "routing.dat"), header=F, rebuild=rebuild, data=routing)

	disc <- disc.catch(dem,
	  									 drn=drn, 
	                     catch=catch,                                         
	                     cuts=cuts,   
	                     reaches=reaches,
	                     cellprops=chan.props,
	  									 cm=hru,
	                     w=weights,
	  									 groups=groups,
	  									 routing=routing,
	                     chan.width=chan.width, 
	                     area.thresh=area.thresh) 

    # strip out invalid groups introduced for some reason!
  #  disc$groups <- disc$groups[1:nrow(disc$w),]
	  disc$dir <- disc.dir
	  disc$chan.width <- chan.width
    disc$area.thresh <- area.thresh
		#disc$routing <- routing
		disc$cuts <- cuts
	#	disc$layer.names <- names(disc$hru)
	  # save results
	  if(!is.null(dn.out)){    
	    try(write.disc(disc, dn.out), silent=T)
	  }
	#  disc$layer.names <- names(hru)
	  return(disc)
}
