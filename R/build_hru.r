# routines for creating flow allocation matrices, classification rasters and hsu group
# summaries for the Dynamic TOPMODEL
# return a matrix of indexes of<- lls immediately downslope of the given cell in the first column
# and correspoinding flow proportion allocated according to weighted midpoint slope in teh second
DownslopeFlowAllocations <- function(rast, cur,
                                     thresh=0)  # thresh is maximum slope to allow downslope flow
  # +ve value allows flow to go "uphill"
{
  # only consider raster cell that have a classification attached
  rast2 <- raster::setValues(rast, NA)
  rast2[cur]<-rast[cur]
  # ensure there are flow directons for all the elevation cells considered
  rast <- fill.sinks(rast, deg=0.1, silent=FALSE)
  # adjacent cells
  # cur <- cur[which(!is.na(rast[]))]
  adj <- adjacent(rast,cur,directions=8, pairs=TRUE)
  # select only directions with -ve (downslope) flow
  dz <- rast[adj[,2]] - rast[adj[,1]]
  good <- which(!is.na(dz) & dz <= thresh)
  adj <- cbind(adj[good, ], dz[good], NA)

  # divvie up flow direction by cell
  cells <- split(adj, as.factor(adj[,1]))
  ncells <- length(cells)
  #	pb <- txtProgressBar(max=length(ncells), title="flow allocations", style=3)
  #	on.exit(close(pb))
  adj <- lapply(cells,
                function(cell.dirs)
                {
                  # row index
                  #   setTxtProgressBar(pb, which(adj[,1]==names(cell.dirs))/ncells)
                  # rebuild the destination cell matrix
                  adj <- matrix(cell.dirs, ncol=4)
                  # sum
                  dz.cell <- adj[,3]  #adj[i.adj.cell,3]
                  if(all(dz.cell==0))
                  {
                    #browser()
                    # deal with flat areas by allocating equally in all directions
                    p <- 1/length(dz.cell)
                  }
                  else
                  {
                    # calculate weighted averages in each direction out of
                    p <- abs(dz.cell/sum(dz.cell))
                  }
                  adj[,4]<-p
                  return(adj)
                }
  )
  # co-erce back to a table and remove any nulls
  adj <- do.call(rbind, adj)
  # table: first col is source, second destination, third proportion of flow in that direction
  return(adj[,c(1:2,4)])
  # no downslope cells
  return(NULL)
}


# construct a nhru x nreach matrix, the (i,j)th entry gives the proportion of the baseflow out of
# the jth land HRU into the ith reach
build.chan.dist.matrix <- function(dem,
                                   drn,
                                   hru,       # raster of groupings
                                   chan.width=2)
{
  # build a raster identifying all reaches by ID
  reaches <- build.reach.raster(dem, drn, chan.width=chan.width)
  # use this determine how baseflow gets allocated between reaches
  w.chan <- build.flow.dist.matrix(dem, cm=hru, reaches=reaches)
  nreach <- nrow(drn)
  # just the land to channel transitions
  w.chan <- w.chan[-(1:nreach),-((nreach+1):ncol(w.chan))]
  return(normalise.rows(w.chan))
}


do.build.flow.matrix <- function(dem, hru, drn, chan.width=4, fact=2)
{
  message("aggregating....")
  dem <- aggregate(dem, fact)
  hru <- aggregate(hru, fact, fun=modal)
  message("getting reaches")
  reaches <- build_chans(dem, drn, chan.width=chan.width)
  message("building...")
  w <- build.flow.dist.matrix(dem, hru, reaches=reaches)
}

# Create a weighting matrix from a similar dem and raster of cell classifications
#                   raster built from spatial object representing channel. If supplied, it should be a spatialines
#                   data frame with a row for each  reach. In the weighting matrix HSU ids are
#                   created for each reach and inserted before the landscape
#                   HSUs. Landscape cells that contain part of the channel flow to that reach,whatever the local topography
build.flow.dist.matrix <- function(dem, cm,  # landscape units
                                   drn=NULL,
                                   reaches=NULL,
                                   ndp=3,
                                   reach.outputs=TRUE,   # if true row and cols added for transitions out of channel units.otherwise only tarnsitions into channel units from land considered
                                   all=FALSE)
{
  cm <- cm + 1000
  #  dem <- rast.mem.copy(dem)
  #  reaches <- rast.mem.copy(reaches)
  #  cm <- rast.mem.copy(cm)
  # check	that rasters have same dims and resolution
  #	compareRaster(dem, cm, res = TRUE)
  # note: these are the ids that indicate HSU classifications, which may contain
  # information on how the hsu was discretised. The transition matrix
  # assumes that they are in sequential order, which can be obtaied from the sort order
  # as teh discretisation retains the structure of the groupings in the order
  # save original ids
  if(all)
  {
    # include every cell, including those outside catchment
    hsuids <- 1:length(cm)
    cellnos <- hsuids
  }
  else
  {
    # just non-NA
    hsuids <- unique(cm[[1]], na.rm=TRUE)  # 1:max(cm[], na.rm=TRUE)}
    cellnos <- which(!is.na(cm[]))
  }
  #back up unit names excluding the channels
  land.ids <- hsuids-1000
  #reaches <- build.chans(dem, drn, reaches, chan.width=4)
  reachids <- setValues(cm, 0)
  cellprops <- setValues(cm, NA)

  # add in channel cells if
  if(!is.null(reaches)) # & !is.null(drn))
  {
    reachids <- reaches[[1]]
    # proportion of cells occupied by reach
    cellprops <- reaches[[2]]
    # reorder so that reach ids are sequential
    rids <- unique(reaches[[1]])

    #   reachids <- subs(reaches[[1]],
    #                     data.frame(rids, order(rids)))
    # insert a hsu for every reach without duplicates. add prefix to prevent duplication
    # with land units

    hsuids <-  c(1:max(rids), hsuids)
  }

  # reclass raster so sequential ids are used
  cm <- raster::subs(cm, data.frame(hsuids, order(hsuids)))

  cat("Getting downslope flow weights...\n")
  start.tm <- Sys.time()
  down.all <- DownslopeFlowAllocations(dem, cellnos)
  cat(nrow(down.all), "directions processed in ", format(difftime(Sys.time(), start.tm), digits=2), "\n")
  # down.all <- down.all[which(!is.na(down.all[,2])),]

  if(length(down.all)==0)
  {
    warning("no flow paths identified")
    return(matrix(0, nrow=length(hsuids), ncol=length(hsuids)))
  }
  from <- down.all[,1]
  to <- down.all[,2]
  down.all <- cbind(down.all, cm[from], cm[to])

  # add in river transitions
  reach.cells <- which(reachids[]>0)
  ireach <- which(down.all[,2] %in% reach.cells)
  rtrans <- down.all[ireach, ]
  # proportion of flow from these cells into channel (rather than identified HSU transition)
  props <- cellprops[rtrans[,2]]
  # build extra rows two for each cell- river cell transition
  # split flow according to proportion of cell occupied by channel
  riv.props <- props*rtrans[,3]   # third col is proportion
  land.props <- (1-props)*rtrans[,3]
  # replace with updated land transitions and append river transitions
  down.all[ireach,3] <- land.props
  # create new rows. 1 is from cell, 2 dest cell. Replace col 3, prop, with calculated value
  # from HSU teh same, last col is destination HSU id - river ids come before HSU ids
  new.rows <- cbind(down.all[ireach, 1:2], riv.props, down.all[ireach,4], reachids[rtrans[,2]])
  down.all <- rbind(down.all, new.rows)
  # HSU and channel transistion table: from hsu, to hsu (or channel), flow proportion
  #  trans <- data.frame(as.factor(down.all[,4]), as.factor(down.all[,5]), down.all[,3])

  trans <- data.frame(down.all[,4], down.all[,5], down.all[,3])
  trans[,1]  <- as.factor(hsuids[trans[,1]])
  trans[,2]  <- as.factor(hsuids[trans[,2]])
  names(trans)<-c("from", "to", "prop")
  trans <- trans[which(!is.na(trans[,2])),]

  # missing destinations ie units that are not linked to any others
  # cross tabulate i.e collate tranistion between pairs of elemenets into a table nhruxnhru in size
  cat("Cross tabulating inter-group transitions...\n")
  w <- xtabs(prop~from+to, data=trans)

  # add in any missing transitions
  while(length(which(!hsuids %in% colnames(w)))>0)
  {
    imissing <- which(!hsuids %in% colnames(w))[1]
    nm <- hsuids[imissing]
    #needs looking at
    w <- insert.col(w, imissing, 0, nm)

  }

  if(!is.null(reaches))
  {
    colnames(w) <- c(paste0("R", 1:max(rids)), land.ids)
  }
  else
  {
    colnames(w) <- land.ids
  }

  zero.rows <- which(rowSums(w)==0)
  if(length(zero.rows)>0)
  {
    # transitions into nowhere!

    warning(paste0("Nil row sum in flow matrix", paste0(names(w)[zero.rows], collapse=", ")))
    # w <- w[-zero.rows,]

  }
  # w <- signif(w, ndp)
  # round to sensible no. dp and renormalise to rows add to 1

  #

  if(!is.null(reaches))
  {
    nreach <- max(rids)  #might actually be fewer
    if(reach.outputs)
    {
      # insert an identity matrix for channel transistions
      # and route all channel flow via a time delay procedure and /or construct
      # inter channel flow transition matrix to route flows down channel
      rw <- matrix(0, ncol=ncol(w), nrow=nreach)
      rw[1:nreach, 1:nreach] <- identity.matrix(nreach)
      w<-rbind(rw, w)

      rownames(w)<- colnames(w)
    }
    else
    {
      # only the land to river transitions
      w <- w[,1:nreach]
    }
  }
  else
  {
    rownames(w) <- land.ids
  }


  w <- normalise.rows(w)

  return(w)
}


insert.col <- function(mat, i, val=NA, nm=NULL)
{
  if(length(val)==1){val<- rep(val, nrow(mat))}
  res <- cbind(mat[,1:min(i, ncol(mat))], val)
  colnames(res)[ncol(res)]<- nm
  if(i<ncol(mat))
  {
    res <- cbind(res, mat[,(i+1):ncol(mat)])
  }

  return(res)
}

normalise <- function(x)
{
  if(is.vector(x))
  {
    tot <- sum(abs(x), na.rm=TRUE)

    return(ifelse(tot==0,x,x/tot))
  }
  return(x)
}




# follow the path downslope until reaching
get.paths.to.outlet <- function(pths, ipths=NULL, outlet)
{
  if(length(ipths)==0)
  {
    # cat("finished")
    return(NULL)
  }
  res <- ipths
  for(ipth in ipths)
  {
    inxt <- which(pths$NODE_B==pths[ipth,]$NODE_A)
    res <- c(res, get.upslope.paths(pths, inxt))
  }
  return(unique(res))
}

get.upslope.paths <- function(pths, ipths=NULL)
{
  if(length(ipths)==0)
  {
    # cat("finished")
    return(NULL)
  }
  res <- ipths
  for(ipth in ipths)
  {
    inxt <- which(pths$NODE_B==pths[ipth,]$NODE_A)
    res <- c(res, get.upslope.paths(pths, inxt))
  }
  return(unique(res))
}


# straight line distance. could scale up to get a nearer estimate
simple.dist.to.outlet <- function(dem, outlet, scale.fact=1.5)
{
  if(!is.null(outlet))
  {
    dists <- distanceFromPoints(dem, xyFromCell(dem, outlet))+ dem-dem
    return(dists*scale.fact)
  }
}

# determine cell in region of cell, if supplied, or lowest cell in DEM if not, that has
# the most upslope connectivity
locate.outlet <- function(dem, cell=NULL)
{
  bound <- raster::boundaries(dem)
  bound[bound==0] <- NA
  # only cells on edge
  outlet <- which.min((dem*bound)[])

  return(outlet)

  adj <- adjacent(dem,cell,directions=8,pairs=FALSE, include=TRUE)

  ndest <- sapply(adj,
                  function(x)
                  {
                    adj <- adjacent(dem,x,directions=8,pairs=FALSE)
                    upslope <- adj[which(dem[adj]>dem[x])]
                    ndest <- length(upslope)
                  }
  )

  outlet <- adj[which.max(ndest)]
  return(outlet)

}

# flow.lens.2 <- function(dem, cells=NULL, dest=NULL, samp=30)
# {
#   dem <- fill.sinks(dem, deg=0.5, silent=FALSE)
#   dir <- terrain(dem, "flowdir")
#   if(is.null(cells))
#   {
#     cells <- which(!is.na(dem[]))
#   }
#   cells <- sample(cells, size=30)
#   pb <- txtProgressBar(max=length(cells), style=4)
#   i <<- 1
#   pths <- sapply(cells,
#          function(cell)
#         {
#           pth <- flowPath(dir, cell)
#           i <<- i + 1
#           setTxtProgressBar(pb, i)
#           return(pth)
#
#         })
#   if(!is.null(dest))
#   {
#     dests <- lapply(pths, function(pth) { pth[length(pth)]==dest})
#   }
#
#
# }

# MatchClass <- function(x, classes)
# {
# 	return(class(x)[1] %in% classes)
# }

# thresh=2/100,   # threshold hsu area contrib
# remove.areas    Logical If TRUE remove regions below the area threshold, otherwise attempt to combine with adjacent groups until reaching the threshold
# burn.hrus       Additional layers that are "stamped" into the discretisation. Ignore their size
combine.groupings.2 <- function(dem,
                                layers=list(),
                                catch=NULL,
                                chans=NULL,
                                cuts=c(a=5),
                                burn.hrus=list(),
                                river.cells.na=FALSE,
                                thresh=2/100,   # threshold hsu area contrib
                                remove.areas=TRUE,
                                renumber=FALSE
) # if TRUE then groups are strictly equal in plan area
{
  if(is.null(catch))
  { # has to be at least one layer supplied
    layers <- delete.NULLs(layers)
    catch<- raster::stack(layers)
  }

  if(!is.null(chans) & river.cells.na)
  {
    # remove river cells from calcs
    ichan <- which(chans[[1]][]>0)
    catch[ichan] <- NA
  }

  # select names in stack matching discretisation. apply in order of cuts
  nms <- intersect(names(cuts), names(catch))

  if(length(nms)==0){stop("No layers found in catchment raster stack matching names of cuts to be applied")}

  # if(length(nms)<length(names(nms.provided)))
  # {
  #   warning("Some names in catchment layers, ", names(catch), ", not found in cut names, ", names(cuts))
  # }
  #
  # default is just one HSU representin entire catchment
  # cm <- dem-dem+101

  crit <- NULL
  # cut each band according to the the number of breaks specified for that layer
  # then combine into a single raster whose values indicate the group index for each cell
  cm <- NULL
  layer.vals <- NULL
  ilayer <- 1

  #
  tab.vals <- NULL

  for(nm in nms)
  {
    message("Discretising layer ", nm)
    layers.cut <- raster::cut(catch[[nm]], breaks=cuts[[nm]]) #, labels=FALSE)
    if(is.null(layer.vals))
    {
      layer.vals <- layers.cut
    }
    else
    {
      layer.vals <- addLayer(layer.vals, layers.cut)
    }
    # encoding the values
    ids <- layers.cut*(10^ilayer)
    if(is.null(cm))
    {
      cm <- ids
    }else
    {
      cm <- cm + max(ids, 0, na.rm=TRUE)
    }
    ilayer <- ilayer+1
  }
  # supress groups that occupy too small an area
  if(remove.areas)
  {
    ibad <- locate_invalid_groups(cm, thresh)
    if(length(ibad)>0)
    {
      cm[which(cm[] %in% ibad)] <- NA
    }
  }
  else
  {
    ids <- merge.groups(cm, thresh,
                        remove = remove.areas)
    cm[which(cm[] %in% ids)] <-NA
  }
  if(renumber)
  {
    # renumber so HRU ids are in sequential order
    subs2 <- data.frame(cbind(unique(cm, na.rm=TRUE), 100+order(unique(cm, na.rm=TRUE))))
    cm <- subs(cm, subs2)
  }

  # adding layers that get stamped into discretisation

  names(cm)<-"HRU"
  # add in the dem and each original layer plus its discretised layer
  #
  cm <- addLayer(cm, catch)


  return(cm)
}

# use the lengths and dem to obatin a 3-band grouping foet he catchment
# then apply the no. custs to teh corresponding layer. Combine to
# produce a final HSU classification
# if channels are supplied then remove the river cells from the calculation
# spatial extent of hrus may supplied explicitily via the optionally named list hrus
combine.groupings <- function(dem,
                              layers=list(),
                              catch=NULL,
                              chans=NULL,
                              cuts=c(a=5),
                              hrus=list(),  # additional layers
                              river.cells.na=FALSE,
                              thresh=2/100,   # threshold hsu area contrib
                              equal.areas =FALSE ) # if TRUE then groups are strictly equal in plan area
{
  if(is.null(catch))
  { # has to be at least one layer supplied
    layers <- delete.NULLs(layers)
    catch<- raster::stack(layers)
  }

  if(!is.null(chans) & river.cells.na)
  {
    # remove river cells from calcs
    ichan <- which(chans[[1]][]>0)
    catch[ichan] <- NA
  }

  # select names in stack matching discretisation. apply in order of cuts
  nms <- intersect(names(cuts), names(catch))

  if(length(nms)==0){stop("No layers found in catchment raster stack matching names of cuts to be applied")}

  # default is just one HSU representin entire catchment
  cm <- dem-dem+101

  ints <- list()
  #if(!all(names(cuts) %in% names(catch)))
  # apply hsu id for point by combining the cut value in each layer


  for(nm in nms)
  {
    cm <- addLayer()
    for(id in raster::unique(cm))
    {
      idcells <- which(cm[]==id)
      n.cut <- cuts[[nm]]
      # cut cells already in this grouping according to the sublevel
      if(is.null(n.cut))
      {
        stop(paste(nm, " specified as cut variable but no corresponding layer found"))
      }
      if(n.cut>1)
      {
        laycutvals <- cut(catch[[nm]][idcells], n.cut, labels=FALSE)
      }
      else{
        # one cuts only so return just when a nin-NA values existin
        laycutvals <- as.numeric(catch[[nm]][idcells]>=0)
      }
      # build new id from top level category plus sub level
      cm[idcells]<-10*cm[idcells] + laycutvals
    }
  }
  # need this to distinguish channel and land HSUs
  if(max(cm[],na.rm=TRUE)<100)
  {cm<-cm+100}

  # merge smaller groups
  cm <- merge.groups(cm, thresh)
  cm <- merge.groups(cm, ids=rev(unique(cm)), thresh)

  # renumber
  subs2 <- data.frame(cbind(unique(cm, na.rm=TRUE), 100+order(unique(cm, na.rm=TRUE))))
  cm <- subs(cm, subs2)

  names(cm)<-"HRU"
  # add in the dem and each discrteised layer
  cm <- addLayer(cm, catch)

  return(cm)
}

# if discrete HRUs supplied (as rasters) stamp these into the discretisation
burn_layers <- function(cm, burn.hrus=list(), exp=10)
{
  id <- max(unique(cm[]), na.rm=TRUE)

  # which power of ten to use to create IDs
  pow <- trunc(log10(id))+1

  for(hru in burn.hrus)
  {
    message("Burning in layer ", names(hru))
    vals <- unique(hru)
    vals <- vals[which(vals>0)]
    # assuuming that the values are < 10 otherwise coukd get confusing
    if(max(vals)>=exp)
    {
      # hard stop!
      stop("Maximum value in layer > exponent of ", exp, " increase to ", exp*10)
    }
    for(val in vals)
    {
      # order of 10 greater to identify the
      id <- 10^pow*val
      icell <- which(hru[]==val)
      cm[icell]<- id
    }


    pow <- pow + 1
  }
  return(cm)

}

get_group_bounds <- function(cm)
{
  nms <- setdiff(names(cm), "HRU")
  hru <- cm[["HRU"]]
  # determine set bounds
  bnds <- NULL
  for(nm in nms)
  {
    cats <- split(cm[[nm]][], hru[])
    cats <- lapply(cats, range, na.rm=TRUE)
    cats <- do.call(rbind, cats)
    colnames(cats)<-paste(nm, c("min", "max"), sep=".")
    bnds <- cbind(bnds, cats)
  }

  return(bnds)
}

merge.groups <- function(cm, thresh, ids=unique(cm), remove=FALSE)
{
  # ids <- unique(cm)
  # id mapping
  map <- data.frame()
  i <- 1
  ntot <- length(which(!is.na(cm[])))
  while(i <= length(ids))
  {
    id <- ids[i]
    tot <- length(which(cm[]==id))
    j<-1
    map <- rbind(map, c(id, id))
    while((i+j)<=length(ids) & (tot/ntot)<thresh)
    {
      idj <- ids[i+j]
      tot <- tot + length(which(cm[]==idj))
      map <- rbind(map, c(idj, id))
      j<-j+1
    }
    # keep collecting until reaching threshold
    i<- i+j
  }
  ids <- rev(ids)
  if(remove)
  {
    # just return the HRU ids so they can be removed from the discretisation altogether
    return(ids)
  }
  return(subs(cm, map))
}

# locate groups that occupy less than the given threshold of the
# catchment discxretisation
locate_invalid_groups <- function(cm, thresh=2/100)
{

  iall <- length(which(!is.na(cm[])))
  ivals <- unique(cm)
  counts <- tabulate(cm[])[ivals]
  props <- counts/iall
  return(ivals[which(props < thresh)])
}


mean.na <- function(x){return(mean(x, na.rm=TRUE))}

# shif the supplied raster so its BL corner is coincident woth the given origin
reset.origin <- function(rast, origin=c(x=0,y=0))
{
  shift(rast, origin[1]-extent(rast)@xmin, origin[2]-extent(rast)@ymin)

}

# group summary table with optional raster of cell proportions available for land
# optionally supply raster with proportions of cells occupied by land: defaults to 1
# if no channel
build.hru.table<- function(cm,
                           dem,
                           reaches=NULL,
                           cellareas=cm-cm+1,  # props occupied by land 0-1
                           catch=NULL)
{
  if(is.null(catch))
  {
    catch=cm[[2:nlayers(cm)]]
    # assume that catchment discretistaion info is passed via the other layer of
    # the multi-band rasteres
    cm <- cm[[1]]
  }
  a.atb <- NULL
  if(all(c("a", "atb") %in% names(catch)))
  {
    a.atb <- catch[[c("a", "atb")]]
  }
  ids <- unique(cm[[1]])
  # handle outside the class matrix
  cellareas[which(is.na(cellareas[])&!is.na(cm[]))]<-1
  # maximum plan area of land within each cell
  maxCellArea <- xres(cm)*yres(cm)

  cat("Building areas...")
  areas<- sapply(ids,
                 function(id)
                 {
                   cm.props <- cellareas[]*(cm==id)[]
                   #       browser()
                   sum(cm.props[], na.rm=TRUE)* maxCellArea
                 }
  )

  # add in river reaches, removing the area covered by channel from the
  # corresponding groups
  if(!is.null(reaches))
  {
    # determine land areas occupied by each channel and insert ids and areas
    # at head of group table
    chans <- zonal((1-cellareas)*maxCellArea, reaches, fun=sum) # sum the areas of cells classed by channel id multiplied by proportion ocuupied for edach
    # two colums: id, area (nas removed)
    areas <- c(chans[,2], areas)
    ids <- c(chans[,1], ids)
    # 		ids <- c(rids, ids)
  }

  nchans <- nrow(chans)
  
  # total catchment area (includes reaches)
  totArea <- sum(areas)

  # reordering so that river reaches appear first. add 100 to distinguish reaches from land hsus
  #	orders <- c(100+((nchan+1):ngroups), 1:nchan)

  # maximum flow is when unit reaches saturation. Transmissivity is lnT0 at this point and flow out
  # per unit contour is lnT0 * dhdt. Using local slope as proxy. Sum these and take average over entire area
  # as an very crude approximation to maximum.
  slope <- terrain(dem, opt="slope") #
  # default aggregation function is mean
  sbar <- zonal(slope, cm)[,2]

  # build table, first two columns identify the units
  groups <- data.frame("id"=ids,
                       "tag"=ids,
                       "chan.no"=NA,
                       "order"=1:length(ids),
                       "area_pc"=round(100*(areas/totArea),2),
                       "area"=round(areas),
                       "sbar"=c(rep(1e6, nchans), sbar))

  #	groups <- cbind(groups, "atb.bar"=0)
  # add other default values
  groups <- add.upslope.areas(groups, dem, cm, a.atb=a.atb,
                              area_pcs=cellareas)
  # average of TWI
  groups$atb.bar <- round(groups$atb.bar, 2)

  # which rainfall / pe record is associated with each HRU
  groups <- cbind(groups, "gauge.id"=1)

  # not used?
  groups <- cbind(groups, "catch.id"=1)
  add.par <- def.hsu.par()
  nms <- setdiff(names(add.par), names(groups))
  add.par <- add.par[nms]
  pars <- data.frame(matrix(rep(add.par, nrow(groups)), byrow=TRUE, nrow=nrow(groups)))
  colnames(pars) <- nms

  groups<- cbind(groups, pars)
  groups <- apply(groups, MARGIN=2, FUN=function(x){unlist(x)})

  #row.names(groups) <- groups[,1]
  return(data.frame(groups, 	row.names=groups[,1]))
}

# locate nearest aws to each group and return index
# add.nearest.gauge <- function(groups, hru.sp, drn, gauges)
# {
#   ids <- groups$id
#   #  groups <- sort(groups$hru.sp)
#
#   for(i.group in 1:nrow(groups))
#   {
#     id <- groups[i.group,]$id
#     hru.geom
#     # locate the geometry associated with the group
#     hru.geom <- hru.sp[which(hru.sp$HRU==id),]
#     cent <- rgeos::gCentroid(hru.geom)
#     # locate current max
#     dist <- rgeos::gDistance(cent, gauges[i.gauge,])
#     for(i.gauge in 1:nrow(gauges))
#     {
#       if(rgeos::gDistance(cent, gauges[i.gauge,])<dist)
#       {
#         groups[i.group,]$gauge.id  <- i.gauge
#       }
#     }
#   }
#   hru.sp@data <- groups
#   return(hru.sp)
# }

# add total upslope area and mean topographic index to groups info
add.upslope.areas <- function(groups, dem, class.m, a.atb=NULL,
                              area_pcs=round(dem/dem))   # optional ratser of cell areas occupied by land
{
  if(!(is.null(dem) | is.null(class.m)))
  {
    if(is.null(a.atb))
    {
      # mean ln(a/tan(b))
      a.atb <- upslope.area(dem, atb=TRUE)
    }
    # uncomment to remove cells containg channel from analysis
    area_pcs[area_pcs<1]<- NA
    # deal with cells partly ocuppied by river channel
    atb.adj <- a.atb[["atb"]]+log(area_pcs)
    # zonal statistics - mean is default. Values adjusted for any cells containing
    # channel
    atb <- zonal(atb.adj, class.m, "mean")  # deals with nas

    # specific discharge per unit recharge [-] assuming steady state.
    # a measure of the relative yield of the area?
    #  sigma.a <- zonal(raster::setValues(dem,a.atb$area), cm, "mean")  # this must have the same first row
    for(row in 1:nrow(atb))
    {
      id <- atb[row,1]
      indx <- which(groups$"id"==id)
      if(length(indx)==0)
      {
        warning(paste("index ", id, "from class matrix not found in groups table"))
      }
      else
      {
        #  cat(id, "\t", indx, "\n")
        # this assummes that every id in the raster has a corresponding id in the table....
        groups[indx,"atb.bar"]<- atb[row,2]
        # q over r
        # groups[indx,"sigma.a"]<- sigma.a[row,2]/groups[indx,]$area
      }
    }
  }
  return(groups)
}

mem.copy <- function(rast)
{
  if(raster::inMemory(rast)){return(rast)}
  #  rast.copy <- stack(rast)
  layers <-
    lapply(names(rast),
           function(ln)
           {
             layer <- rast[[ln]]
             raster::setValues(layer, getValues(layer))
           }
    )
  if(length(layers)>1)
  {
    return(stack(layers))
  }
  else
  {
    return(layers[[1]])
  }
}



get.def.thresh <- function(area.thresh=1)
{
  return(area.thresh)
}

get.defs <- function()
{
  return(list(chan.width=2, area.thresh=1))
}


# create a new discretisation and add to project's list. if it exists already then just add
# to list of discretisations, unless rebuild is true in which case rebuid the components
# add.disc <- function(proj, cuts=NULL, i.disc=0, rebuild=FALSE, chan.width=2,
# 										 ...)
# {
# 	# proj <- merge.lists(proj, list(...))
#
# 	# build names from extra parameters
# 	cuts <- merge.lists(cuts, list(...))
#
# 	# name wil be infered from cuts
# 	dn <- disc.dir.name(cuts, dn=proj$disc.dir)
# 	catch <- NULL
# 	try(catch <- build.proj.catch(proj))
# 	# rebuild <- file.exists(dn)
# 	if(rebuild){message(paste("Rebuilding files in ", dn))}
# 	disc <- NULL
# 	disc <-disc.from.dir(dem=proj$dem,
# 													 drn=proj$drn,
# 													 reaches=proj$reaches,
# 													 routing=proj$routing,
# 													 catch=catch,
# 													 dn=dn,
# 													 cuts=cuts,
# 													 rebuild=rebuild,
# 													 chan.width=chan.width,
# 													 agg=agg,
# 												#	 area.thresh=proj$area.thresh,
# 												...)
#
# 	if(!is.null(disc))
# 	{
# 		if(is.null(i.disc) | i.disc <=0 | i.disc > length(proj$disc))
# 		{
# 			# return new discretisation
# 			return(disc)
# 		}
# 		# same cuts as any other?
# 		proj$disc[[i.disc]]<- disc
# 		return(proj$disc[[i.disc]])
# 	}
# 	else
# 	{
# 		warning("Adding discretisation ", paste(cuts, collapse=","), "failed")
# 	}
# }




#*******************************************************************************
# TOPMODEL helper routines
#*******************************************************************************
# discretisation of catchment according to topographic index
disc.topidx <- function(dem, riv, nbreaks)
{
  atb<-upslope.area(dem, atb=TRUE)[["atb"]]
  if(!is.null(riv))
  {
    atb[which(riv[]>0)]<-NA  # ignore river cells
  }

  atb.hist <- hist(atb, breaks=nbreaks, freq=FALSE)
  tot <- sum(atb.hist$counts)
  # second col is for proportion occupied - final entry is zero for count of cells > final break
  topidx.frame <- cbind(atb=atb.hist$breaks, prop=c(atb.hist$counts/tot, 0))
  return(topidx.frame)
}


# build an approximate routing table from the dem
#"Flow is routed through a delay function which represents the time spent in the
# channel system. The parameter delay is used for this. Delay is a matrix with 2
# columns. The first column gives the cumulative relative area. The second column
# gives the average distance towards the outlet (m)."
get.routing.table <- function(dem, nreach=5)
{
  dem <- raster::setValues(dem, topmodel::sinkfill(as.matrix(dem), degree=0.1, res=10))

  fl <- raster::setValues(dem, topmodel::flowlength(as.matrix(dem)))*xres(dem)

  routing.hist <- hist(fl, breaks=nreach)
  n <- sum(routing.hist$counts)
  # build the table@ first column is disatnce to outlet, second the cumlative area
  routing <- cbind(prop=cumsum(routing.hist$counts)/n, d=routing.hist$mids)
  return(routing)
}
#*******************************************************************************

# construct a name for a directory in whci to put files for catchment discretisation
disc.dir.name <- function(cuts, dn="", area.thresh=0)
{
  # convert to %age
  #  if(area.thresh<1){area.thresh <- area.thresh*100}
  return(file.path(dn, paste0(names(cuts),"=", cuts, collapse=",")))
}


is.stack <- function(obj)
{
  if(is.null(obj)){return(FALSE)}
  if(inherits(obj, "RasterStack") | inherits(obj, "RasterLayer") | is(obj, "RasterBrick")){return(TRUE)}
  return(FALSE)
}

# wrapper to create all input for Dynamic TOPMODEL run given DEM and a set of
# breaks on eff dist to channel, uplsloploe area and slope
# also builds information for "normal" TOPMODEL run

aggregate.null <- function(rast, fact, fun=mean)
{
  fact <- round(fact)
  if(is.stack(rast) & fact > 1)
  {
    rast <- aggregate(rast, fact, fun=fun)
  }
  return(rast)
}

get.flow.distribution.matrix <- function(dem, cm, reaches, sf=3)
{
  agg <- max(1,round((ncell(dem)/1e6)))
  if(agg>1){
    message(paste0("Aggregating dem by factor of ", agg))
  }
  dem <- aggregate.null(dem, agg)
  cm <- aggregate.null(cm, agg, fun=modal)
  reaches <- aggregate.null(reaches, agg)

  message("Creating flow transistion matrix....")
  # flow transistion matrix including transfers to river
  w <- build.flow.dist.matrix(cm=cm, dem=dem, reaches=reaches)
  # always require some kind of channel identification raster even if constructed from DEM
  message("Creating channel routing matrix....")
  # construct flow connectivity graph using the reach identification raster previously loaded
  # or built, using reach ids as the "classification" and flow weightings from dem.
  adj <- build.flow.dist.matrix(cm=reaches[[1]], dem=dem, reaches=NULL)

  # clear the weights for all river hsus, then add the adjacency matrix
  # overwrite channel entries of weighting matrix with river connections
  w<- as.matrix(w)
  ichan <- 1:nrow(adj)
  w[ichan,]<-0

  w[ichan, ichan] <-  as.matrix(adj)
  w <- signif(w, sf)
  row.names(w)[ichan]<- paste0("R", row.names(adj))
  ids <- row.names(w)

  rownames(w)<- ids
  colnames(w)<- ids
  return(w)
}

#
# create.reach.info <-  function(dem, w, reaches)
# {
#   # destinations after 100 cell-cell iterations
#   w100<-matrix.power(w, 100)
#
#   # consider just land - river transitions
#   w100<- as.matrix(w100[(nchan+1):nrow(w),1:nchan])
#
#   # normalise to a probability distribution - asuume all flow enters a reach
#   props <- round(NormaliseVector(colSums(w100)), 3)
#
#   # flow lengths to outlet. doesn't work very well without first filling sinks
#   flowlens <- raster::setValues(dem, flowlength(as.matrix(dem))*xres(dem))
#
# 	# in-channel distances determined by zonal mean of flow lens for dem cells containing the channel(s)
# 	chanlens <-zonal(flowlens, reaches[[1]])
#
#   # construct reach info table. Compatible with TOPMODEL if props are cumulative
#   reach.info <- data.frame("dist"=round(chanlens[,2]), "prop"=props)
#   return(reach.info)
# }
#
#




# not needed: Laplacian filter for raster focal function
#fact <-1/sqrt(2)
#downslope.contour.filter <- matrix(c(fact, 1, fact, 1,1, 1,fact,1,fact), nrow=3, byrow=TRUE)

FindNext <- function(x)
{
  # browser()
  cur <- x[5]
  if(is.na(cur))
  {
    return(NA)
  }
  # weight straight directions more heavily
  dz <- x[5]-x
  return(which.max(dz))
}


