create.proj <-
function(data.dir,  # location for DEM, drn etc
											 id=data.dir,
											 cuts=NULL,      # cuts applied for first discretisation
											 area.thresh=1,
											 chan.width=1,
											 disc.dir=file.path(data.dir, "disc"),   #  root location for discretisations      
											 obs = NULL,
											 obs.dir=fp(data.dir, "obs"),    # obseravtions e.g rain, pe and discharge. can be from multiple sites identifeid by sites
											 #rebuild=F,
											 ...)
{
#	sink(type = "message")
#  on.exit(sink(NULL))
	if(!file.exists(data.dir))
	{
		if(readline(paste("Specified location ", data.dir, " not found, create?"))=="y")
		{
			dir.create(data.dir, recursive=T)
		}
		else
		{
			stop("Can't find directory")
		}
	}
	proj <- new.project()
	proj$name <- id
	proj$dir<-data.dir
	# root folder where discretisation live
	proj$disc.dir<-disc.dir
	# add in extra parameters 
	proj <- merge.lists(proj, list(...))
	
	#  try(proj$dem <- rast.mem.copy(raster(file.path(data.dir, "dem.tif"))))
	#try(proj$drn <- rgdal::readOGR(data.dir, "drn"))
	# iterate through any shape files located and add verbatim
	for(shp in dir(data.dir, "*.shp"))
	{
		# name without extention
		shp.nm <- sub("*.shp", "", shp)
		cat("Loading shape file ", shp.nm, "...")
		try(proj[[shp.nm]] <- rgdal::readOGR(data.dir, shp.nm))
		if(!is.null(proj[[shp.nm]])){cat("done\n")}
		else{cat("failed\n")}
	}
	
	# might be a routing table applicable to every dicsretisation
	proj$routing <- load.disc.data(file.path(data.dir, "routing.dat"), header=F, rebuild=F)
	# catchment layers that can be used to provide data for discretisations
	catch <- NULL
	# add raster verbatim and copy to memeory to prevent hard coding disk referencve
	for(rast.fn in dir(data.dir, "*.tif", full.names=T))
	{
		rast <- NULL
		try(rast <- raster(rast.fn))
		#   try(rast <- rast.mem.copy(rast))
		# name for list element is raster file name with out path and extension
		rast.nm <- sub("*.tif", "", basename(rast.fn))    
		proj[[rast.nm]] <- rast
		message(paste("Adding raster ", rast.fn))
		if(is.null(catch))
		{
			catch <- stack(rast)
		}
		else
		{
			try(catch <- addLayer(catch, rast))
		}
		#  try(proj$reaches <- raster(file.path(data.dir, "reaches.tif")))
	}
	proj$catch <- catch
	# build
	#  try(proj$dem <- raster::setValues(proj$dem, sinkfill(as.matrix(proj$dem), res=xres(proj$dem), degree=0.1)))
	# any files ending with ".par" are assummed to contained R source that can be loaed and parsed as is
	par.fn <- dir(data.dir, "*.par$", full.names=T)
	for(fn in par.fn)
	{
		#  envir <- as.environment(proj)
		cat("Trying settings from ", fn, "...\n")
		try(source(fn, local=proj, echo=T), silent=F)    
	}
	# rainfall, pe and observed flows
	proj <- load.obs(proj, obs.dir)
  # attempt to set the range 
  proj <- get.sim.range(proj)
	# load cuts and other discretisation info from subdirectories of specified directory or use default location "disc"
	# list of discretisations excluding any observations 
	dns <- setdiff(list.dirs(disc.dir, recursive=F), c(disc.dir, obs.dir))
	proj$disc <- lapply(dns,
											function(dn)
											{
												disc <- NULL
												cat("trying directory ", dn, "...")
												# look in every subfolder of the discretisation root dir and attempt to load a 
												# discretisation. if it fails the error will be returned in disc
												try(disc <- disc.from.dir(dem=proj$dem, drn=proj$drn, 
																									reaches=proj$reaches, dn=dn, cuts=NULL,
																									#rebuild=rebuild,... 
																									chan.width=chan.width,
																									catch=catch,
																									routing=proj$routing,
																									area.thresh=area.thresh, dn.out=NULL)
												)
												return(disc)
												#if(!is.null(disc)){cat("success")}
											}
	)
	# add in any new discretisations specified via teh cuts function arguments
	if(!is.null(cuts))
	{
		proj <- add.disc(proj, cuts, ...)   
	}
  # remove NuLL and invalid discs then order by no. HRUs
  try(proj$disc <- clean.disc(proj$disc))
  # vector of HRU counts
  try(proj$hru.counts <- get.disc.counts(proj$disc))
	# locate errors when loading discs
#	try(errs <- lapply(proj$disc, function(disc){if(!is.list(disc)){return(disc)}}))
	# remove failed disc loaded and repiort
#	cat("\n", length(proj$disc), " discretisation(s) loaded\n")
	
	# any optional parameters supplied will override the default settings
	proj <- merge.lists(as.list(proj), list(...))
	proj$disp.par$title.main <- proj$name
	
	return(as.list(proj))
}
