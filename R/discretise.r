#' Discrete a catchment into hydrological response units (HRUs)
#'
#' @description Discrete a catchment into a set hydrological response units (HRUs) according to any number of landscape layers and cuts
#' @details This applies the given cuts to the supplied landscape layers to produce areal groupings of the catchment.
#' @export discretise
#' @import raster
#' @param layers A multi-band raster (stack) comprising the catchment data. This should be in a projected coordinate system (or none) and have regular cells. The first layer should be the elevation raster, and subsequent (named) layers should supply the landscape data drawn in to create the discretisation
#' @param cuts A list of cuts of the form layer_name=number. Each name should correspond to a layer name in the layers parameter.
#' @param order.by Name of layer whose values will be use to sort the response units, in decreasing order. Defaults to the name of the first cut
#' @param area.thresh	Minimum area for response units, expressed as a percentage of the catchment plan area, excluding channel cells. Areas smaller than this are aggregated with adjacent areas until exceeding the threshold area
#' @param chans	Raster containing channel reach locations, of the same dimensions and resolution of the DEM and other catchment layers. The reaches should be numbered sequentially and any areas not containing part of the channel should be NA. If a second band is supplied with values 0-1 then this is taken to be the proportion of the corresponding non-zero cell occupied by the channel. If this layer is not present then the proportion is inferred from the channel width as p = min(1, chan.width/xres(dem))
#' @param chan.width Channel width, in same units as DEM. Only used if chans doesn't contain a layer to specify the proportion of each river cell comprised of the channel.
#' @param burn.hrus list  Named list of geometries (supplied as rasters) to burn into discretisation of HRUs that will be stamped onto the classification. Overrides any classification already defined
#' @param remove.areas  Boolean Whether to remove areas that fall into classes with smaller areal contribution than the supplied threshold (default FALSE)
#' @param renumber  Boolean Renumber HRUs after discretisation so that their IDS are in numerical order (default TRUE)
#' @param riv.cells.na Boolean Remove river cells from discretisation (default FALSE)
#' @param hrus list Unused, maintained for backward compatibility
#' @param order  Boolean Reorder HRUs after discretisation so that those with highest TWI come first (approximate to distances from channel). Default FALSE

#' @return A list comprising the following:
#' @return groups A data frame whose rows comprising the names, plan area and model parameters of each response unit. See Beven and Freer (2001) and Metcalfe et al. (2015) for a description of these parameters
#' @return weights	Flux distribution (weighting) matrix for routing of subsurface flow downslope through the response units. If n is the number of response units (including channel "unit(s)") this is an n x n matrix.
#' Row sums should thus always add to 1. The elements of the i-th row give the proportion of flow directed from response unit i to the other units
#' @return cuts list Cuts applied to produce these HRUs
#' @return area.thresh  Area threshold specified
#' @return layers Multi-band raster comprising the the original rasters that the specified cuts were applied to produce the discretisation; the channel network
#' @return chans The channel raster
#' @return hru	The resultant response unit locations
#' @examples
#' # Landcover and soils are fairly homogenous throughout the Brompton catchment.
#' # Due to the extensive artifical sybsurface drainage discharging directly into 
#' # the channel it is hypothesied that the storm response is largely mostly controlled 
#' # by proximity to the network. A simple discretisation according to flow distance 
#' # from the nearest channel thus appears to capture the dynamics without introducing
#' # unnecessary complexity.
#'\dontrun{
#' require(dynatopmodel)
#'
#' data(brompton)
#'
#' chans <- build_chans(brompton$dem, drn=brompton$drn, chan.width=2)
#' sort by distance from the channel network, but want areas closest the channel to come first
#' layers <- addLayer(brompton$dem, 2000-brompton$flowdists)
#' disc <- discretise(layers, cuts=c(flowdists=10), chans=chans, area.thresh=0.5/100)
#' rm(chans)
#' rm(layers)
#' write.table(disc$groups, sep="\t", row.names=FALSE)
#'}

discretise <- function(layers,
                       chans,
                       cuts=list(a=10),
                       area.thresh=2/100,
                       order.by=names(cuts)[[1]],
											 riv.cells.na=FALSE,
											 renumber=FALSE,
											 order=FALSE,
											 burn.hrus=NULL,
                       chan.width=5,
										   remove.areas=TRUE,
											 hrus=NULL)
{
  dem <- layers[[1]]
  catch <- layers

  #  check to see if rasters are identical in resolution and extent
  compareRaster(dem, chans)

  if(is.null(area.thresh)){
    area.thresh<- get.defs()$area.thresh
  }

  if(is.null(chan.width)){
    chan.width<- get.defs()$chan.width
  }

  if(area.thresh>=1){area.thresh<-area.thresh/100}

  if(nlayers(chans)>1)
  {
    cellprops <- chans[[2]]
  }
  else
  {
  	# calculate the proportion of the cells occupied by the channel
  	cellprops <- chans[[1]]- chans[[1]]
    cellprops <- cellprops + min(1, chan.width/xres(chans))
  }

  nchan <- length(unique(chans[[1]], na.rm=TRUE))
  #  nchan <- nrow(drn)
  # channel identifiers
  ichan <- 1:nchan

  message("Combining layers...")
  # cuts up catchment into discrete HRUs according to the cuts supplied
  # note that cuts that refer to layers that will be burned are ignored
  cm <- combine.groupings.2(dem,
  												catch=catch,
  												chans=chans,
                          cuts=cuts,
													river.cells.na=riv.cells.na,
  												thresh=area.thresh,
													renumber = renumber,
											#		burn.hrus=burn.hrus,
													remove.areas=remove.areas)

  # default sort order is using upslope area
  if(!"atb" %in% names(catch))
  {
  	atb <- upslope.area(dem, atb=TRUE)$atb
  }
  else
  {
  	atb <- catch[["atb"]]
  }

  if(order)
  {
    # reorder by upslope area
    # first layer in landscape is the HRU ID
    zonal.vals <- raster::zonal(atb, cm[[1]])

    #  wetness index by zone (HRU)
    ords <- order(zonal.vals[ ,2], decreasing=TRUE)

    # create sequences for ordering the HRUs
    # ord.args <- lapply(as.list(order.by),
    #                    function(x)
    #                    {
    #                      zonal.vals[, x]
    #                    })
  #  ords <- do.call(order, c(ord.args, decreasing=TRUE))  # by default zone with higher averages appear first
    sub.df <- data.frame(cbind(zonal.vals[,"zone"], zonal.vals[ords,"zone"]))

    cm[[1]] <- subs(cm[[1]], sub.df)

    nms <- names(cm)
    nms[[1]]<- "HRU"
    names(cm)<-nms
  }
  # "stamp in" any extra layers, in the order in which they're supplied (i.e.
  # first element to last, each overwriting the previous)
  # the classifications overwrite any previous classes and are not subject to
  # checking for their szie
  if(length(burn.hrus)>0)
  {
    nms <- names(layers)
    # attempt to add the additional HRU to the layers for the discretisation
    for(layer in burn.hrus)
    {
      compareRaster(layer, dem)
      layers <- addLayer(layers, layer)
    }
    names(layers) <- c(nms, names(burn.hrus))
    cm[[1]] <- burn_layers(cm[[1]], burn.hrus)
  }

  message("Building group info table....")

  # layers that went into discretisation are held in further layers of classification matrix:
  # pass into proc to produce group summary table
  groups <- data.frame(build.hru.table(cm, dem=dem,
                                       reaches=chans[[1]],
                                       cellareas=1-cellprops))

  groups[ichan,"chan.no"] <- ichan
  groups[ichan,"vof"] <- NA
  groups[-ichan,"vchan"] <- NA

  # generate the intergroup flux weightng matrix
  w<-get.flow.distribution.matrix(dem,
                                  cm=cm[[1]],
                                  reaches=addLayer(chans[[1]], cellprops))

  # return a list of group table, the landscape layers used,
  # the channel loactions, HRU spatial distribution and the flow weighting matrix
  return(list(
     groups=groups,
     layers=layers,
     chans=chans,
     cuts=cuts,
     area.thresh=area.thresh,
     hru=cm[[1]],
     weights=w))
}

exists.not.null <- function(obj.name, check.file=TRUE, warn=NULL)
{
  res <- FALSE
  # calling frame / environment, up one level in calling stack, must be at least one
  p.env <- sys.frame(-1)
  # treat the argument as a charcater object name and look in the calling frame
  if(exists(obj.name, where=p.env))
  {
    obj <- get(obj.name, p.env)
    if(!is.null(obj))
    {
      res <- ifelse(check.file,
                    file.exists(obj),
                    TRUE)

    }
  }
  if(!res & !is.null(warn))
  {
    warning(warn)
  }
  return(res)
}


# use catchment area calculated from dem to determine specific discharges from the
#input given in cu.m/sec
convert.to.specific.discharges <- function(proj, q)
{
	if(max(q))
	# catchment area
  a <- with(proj, sum(length(which(!is.na(dem[])))*xres(dem)*yres(dem)))
  # assumme in cu.m/s
  res <- 3600*q/a
  if(max(res, na.rm=TRUE)>1){warning("Very large specific discharges calculated: check input not in mm")}

  return(res)
}

add.layers <- function(proj, reload=FALSE)
{
	nms <-  dir(proj$dir, "\\.shp")
	if(!reload)
	{
		nms <- setdiff(nms, paste0(names(proj), ".shp"))
	}
	#  try(proj$dem <- rast.mem.copy(raster(file.path(data.dir, "dem.tif"))))
	#try(proj$drn <- readOGR(data.dir, "drn"))
	# iterate through any shape files located and add verbatim
	for(shp in nms)
	{
		# name without extention
		shp.nm <- sub("*.shp", "", shp)
		cat("Loading shape file ", shp.nm, "...")
		try(proj[[shp.nm]] <- readOGR(proj$dir, shp.nm))
		if(!is.null(proj[[shp.nm]])){cat("...done\n")}
		else{cat("...failed\n")}
	}
	return(proj)
}

check.time.intervals <- function(proj)
{
  int <- time.interval.intersection(proj$obs$rain, proj$sim.start, proj$sim.end)
  return(length(int)>0)
}

time.interval.intersection <- function(obs, sim.start, sim.end)
{
  int <- which(as.numeric(index(obs))<= as.numeric(sim.end) &
                 as.numeric(index(obs))>= as.numeric(sim.start))
  if(length(int)>0)
  {
    return(index(obs)[int])
  }
  return(NULL)
}

# given a set of observations and a specified run interval
# expand / contrcat the run times to accommodate the observations
fix.run.dates <- function(proj)
{
  obs <- proj$obs$rain

  if(!is.null(obs) & !check.time.intervals(proj))
  {

    warning("No rainfall data within specified run start/ end times: adjusting...")
    len.sim <- proj$sim.start-proj$sim.end

    start <- start(index(obs$rain))
    cat("Setting sim start to ", "\n")
    proj$sim.start <- start
    end <- min(start + len.sim, end(index(obs$rain)))
    cat("Setting sim end to", end, "\n")
    proj$sim.end <- as.POSIXct(end)
  }
  return(proj)
}

aggregate_observations <- function(proj)
{
  try(proj<-fix.run.dates(proj))
  obs <- proj$obs
  # check that the specified start end and end dates contain at least some rainfall data. Other data are
  # less important and take null defaults if not specified
  try(obs$pe <- disaggregate_xts(proj$obs$pe,
                                 ser.start=proj$sim.start,
                                 ser.end=proj$sim.end,
                                 dt=proj$dt, is.rate=TRUE))
  try(obs$rain <- disaggregate_xts(proj$obs$rain,
                                   ser.start=proj$sim.start,
                                   ser.end=proj$sim.end,
                                   dt=proj$dt, is.rate=TRUE))
  # note observed flows required in specific discharge m/hr
  try(obs$qobs <- disaggregate_xts(proj$obs$qobs,
                                   ser.start=proj$sim.start,
                                   ser.end=proj$sim.end,
                                   dt=proj$dt, is.rate=TRUE))

  return(obs)
}


# apply the given parameters to groups in all discretisations
apply.params <- function(proj, params, which=1:length(proj$disc))
{
  if(length(proj$disc)==0){return(proj)}
  params <- params[which(names(params) %in% colnames(proj$disc[[1]]$groups))]
  if(length(params)==0){return(proj)}

  proj$disc[which] <- lapply(proj$disc[which],
                             function(disc)
                             {

                               vals <- matrix(rep(unlist(params),nrow(disc$groups)), nrow=nrow(disc$groups), byrow=TRUE)
                               disc$groups[,names(params)]<- vals

                               return(disc)
                             }
  )
  return(proj)
}


# graphics and text output
gr.on <- function(proj, spatial=FALSE)
{
  return(graphics.on.off(proj,TRUE, spatial))
}

gr.off <- function(proj, spatial=FALSE)
{
  return(graphics.on.off(proj, FALSE, spatial))
}

graphics.on.off <- function(proj, val=TRUE, spatial=FALSE)
{
  proj$disp.par$graphics.show <- val
  if(spatial)
  {
    proj$disp.par$graphics.spatial.show <- val

  }
  return(proj)
}

output.off <- function(proj)
{
  proj$disp.par$text.out <- NULL
  return(proj)
}

output.on <- function(proj)
{
  proj$disp.par$text <- stdout()

}




# plot a set of results associated with a topmodel run
plot.run.response <- function(run,   # dtm run output
                               qresp, # response series
                               evap=NULL,
                               ymax=NULL,
                                show.qobs=FALSE,
                               fn=NULL,
                               lwd=1,
                               lty=1,
                               cols=c("blue", rainbow(ncol(qresp)-1)),
                               title="",
                              start=index(qresp)[1],
                              end=index(qresp)[length(index(qresp))],

                              ...)
{
  sel <- paste0(start, "::", end)
  run$qsim <- run$qsim[sel]
  qresp <- qresp[sel]
  if(length(evap)>0)
  {
    run$evap <- evap
    names(run$evap)<- "ae"
  }
  else
  {
    run$evap <- NULL
  }
  nresp<- ncol(qresp)
  if(!show.qobs)
  {
    run$qobs<-NULL
  }

  plot.run(run, qsim=qresp,
           cols=cols, fn=fn,
           title=title,
           ymax=ymax,
           lwd=lwd,
           lty=lty,
           start=start,
           end=end,
           #legend=FALSE,
         #  legend.col=cols,
           ...)


}


# plot the results of a run using another matrix of q and a source project
plot.q <- function(proj,
                   qresp,
                   evap=NULL,
                   ymax=NULL,
                   fn=NULL,
                   lwd=2,
                   lty=1,
                   cols=rainbow(ncol(qresp)),
                   title="", ...)
{
  run <- proj
  run$disc <- NULL
  run$proj <- proj
  run$qobs <- NULL
  run$rain <- proj$obs$rain

  if(evap==FALSE)
  {
    evap <- NULL
  }
  else if(is.null(evap) & !is.null(proj$obs$pe))
  {
    run$evap <- proj$obs$pe
    names(run$evap)<- "ae"
  }
  nresp<- ncol(qresp)

  plot.run(run, qsim=qresp*1000,
           cols=cols, fn=fn,
           title=title,
           ymax=ymax,
           lwd=lwd,
           lty=lty,
           ...)

}

