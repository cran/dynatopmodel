#' Construct a raster of channel locations from vector or topographic data
#'
#' @description The discretise methods both requires a raster defining the
#'   locations of the channel cells and the proportion of each river cell
#'   occupied by the channel. A detailed river network (DRN) may be available in
#'   vector format and can be used to compute this. If not, the channel location
#'   can be inferred from a spatially-distributed metric, typically the
#'   topographic wetness index.
#'
#' @export build_chans
#' @param dem raster  Elevation raster (DEM) using a projected coordinate system (e.g UTM) and regular grid spacing. Not required if atb raster supplied.
#' @param drn SpatialLines Detailed river network (DRN) in vector (ESRI Shapefile) format. Not required if atb raster supplied.
#' @param chan.width numeric Vector of channel widths, in m, for each reach defined in the DRN. Will be recycled if shorter than the number of channels
#' @param atb raster  Optional raster used as criteria for locating the channel. Typically the value of the topographic wetness index (TWI) determined from the elevations. Should be in a projected coordinate system (e.g UTM) and use a regular grid spacing.
#'
#' For the TWI to be meaningful this raster should have a resolution of a least
#' 30m. It can be calculated using the upslope.area method applied to the DEM
#' and atb=TRUE.
#' @param atb.thresh If atb supplied and DRN null then this specifies the threshold value above which cells are identified as containing part of the channel network
#' @param buffer If using a vector input then buffer the DRN by this width to capture all river cells.
#' @param single.chan If using a vector input then the first raster layer returned contains either 1 for a river cell or NA for a non-river cell. Otherwise the values are the line ids of the channel vector, that typically identify individual reaches
#' @return A two-band raster with the same dimensions as the elevation or ATB raster whose first layer comprises non-zero cells where identified with the channel and whose second layer holds the proportions of those cells occupied by the channel.
#' @references Kirkby, M. (1975). Hydrograph modelling strategies. In Peel, R., Chisholm, Michael, Haggett, Peter, & University of Bristol. Department of Geography. (Eds.). Processes in physical and human geography : Bristol essays. pp. 69-90. London: Heinemann Educational.
#' @examples
#'\dontrun{
#'
#' require(dynatopmodel)
#' data("brompton")
#'
#' chan.rast <- build_chans(dem=brompton$dem, drn=brompton$drn, buff=5, chan.width=2)
#' # show it
#' sp::plot(chan.rast[[1]], col="green", legend=FALSE)
#' }
build_chans <- function(dem,
                        drn,
                        chan.width=1,
												atb=NULL,
                        buffer=10,
                        atb.thresh=0.8,
                        single.chan=TRUE)
{
  if(!is.null(drn))
  {
    # build the reach multi band raster and save
    message("Building raster for channel(s)...")
    reaches <- build.reach.raster(dem, drn, buffer=buffer,
                                  chan.width=chan.width)

    # single channel desired but multi-reach river network supplied
    reaches[[1]] <- reaches[[1]]>0

    # ensure DEM and reaches have same extent ( processing can leave the former larger)
    reaches <- crop(reaches, dem)
    # Estimate proportion of river cells occupied by channel
  #  prop <- min(chan.width/xres(dem), 1)

  }
  else if(!is.null(atb))
  {
     # using the TWI to identify the channel
    reaches <- atb > atb.thresh*max(atb[], na.rm=TRUE)
    # Estimate proportion of river cells occupied by channel
    prop <- min(chan.width/xres(atb), 1)
    reaches[which(reaches[]==0)] <- NA
    cellprops <- reaches*prop
    reaches <- addLayer(reaches, cellprops)
  }
  else
  {
    stop("Provide vector channel data or raster layer to locate channels")
  }

  names(reaches) <- c("chans", "chanprops")
  return(reaches)
}

#############################################################
# raster of reach locations and cell proportion occupied
#############################################################
build.reach.raster <- function(dem, drn,
                               buffer=10,

                               chan.width=1)
{
  # determnine the number of distinct reaches in the reach vector and compare with the chan.width vector
  # if greater then apply the width to all reaches
  drn <- as(drn, "SpatialLines")
  nchan <- length(drn@lines)
    if(length(chan.width) < nchan)
    {
        chan.width <- rep(chan.width, length.out=nchan)
        # have only supplied a single width across catchment
    }
  chan.width <- as.vector(chan.width)
  dem <- dem+0  # now in memory to prevent disk access

    # for reaches wider than cell size, expand the network so that adjacent cells are occupied
    buffer.width <- chan.width/2
    ichan <- which(chan.width>xres(dem))
    if(length(ichan)>0)
    {
      buffer.width[ichan] <- 0
    }
    # create buffer with specified width (on either side)
    drn <- rgeos::gBuffer(drn, width=buffer.width, byid=TRUE)

    reaches <- dem
    reaches[] <- NA
    # one row per channel
    message("Extracting reach cells...")
    rl <- extract(dem, drn, cellnumbers=TRUE)

    # firt column the cell number occupied by the channel, second the reach index, third the width of this channel
    rlw <- lapply(1:length(rl),
                  function(i)
                  {
                    cbind(rl[[i]][,1], i, chan.width[i])

                  })

    rlw <- do.call(rbind, rlw)

    reaches[rlw[,1]] <- rlw[,2]

  # proportions of channel cells occupied by channel
  props <- reaches
  props[rlw[,1]] <-   pmin(rlw[,3]/xres(dem), 1)
  reaches <- addLayer(reaches,props)

  # calculate the cell proportions
  #	prop <- min(chan.width/xres(dem), 1)
  #	cellprops <- reaches*min(chan.width/xres(dem), 1)

  # add a layer with proportions of cell occuppied by channel. estimated by
  # proportion of cell size to channel width, probably close enough
  #	reaches <- addLayer(reaches, (reaches>0)*prop)

  names(reaches)=c("chan", "chanprop")

  return(reaches)
}
