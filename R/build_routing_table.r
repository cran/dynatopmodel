#' Generate a network routing table
#'
#' @description Generates a network width table for a catchment. When passed to the run.dtm routine this will be used to route channel flows to the outlet during a Dynamic TOPMODEL run.
#' @export build_routing_table
#' @details Dynamic TOPMODEL routes channel flow to the outlet by a network-width approach (see Beven, 2012, pp. 97-97). A time-delay histogram is produced using the table. When any flow is distributed to the channel "unit" it is immediately redistributed across future time steps according to the proportions found in the histogram. These flows are then added to future outputs from the model.
#' @author Peter Metcalfe
#' @param dem Elevation raster using a projected coordinate system (e.g UTM) and a regular grid spacing. Areas outside the catchment should be set to NA
#' @param breaks Number of distance intervals
#' @param chans Optional raster of the same dimensions and resolution as the DEM. Non-zero cells in this raster are considered to contain a river channel. If not supplied then flowpaths from the entire catchment area are considered.
#' @param len.fun For large rasters the flow.len function can be very slow and many paths fail to reach a single outlet cell. This applies a simple straight line distance to the outlet to obtain a rough approximation.
#' @param outlet Index of cell or cells identified with the catchment outlet
#' @return A two-column data.frame. Its first column is the average flow distance to the outlet, in m, the second the proportions of the catchment channel network within each distance category.
#'
#' @references  Beven, K. J. (2012). Rainfall-runoff modelling : the primer. Chichester, UK, Wiley-Blackwell.
#' @examples
#' \dontrun{
#' # Create a routing table for the Brompton test case and show histogram
#'
#' data(brompton)
#'
#' tab <- build_routing_table(brompton$dem,
#'   chans=brompton$reaches,
#'   breaks=5)
#' barplot(tab[,2]*100, xlab="Mean flow distance to outlet (m)",
#' ylab="Network Width %", names.arg=tab[,1])
#' }
build_routing_table<- function(dem,
                               chans=NULL,
                               outlet=NULL,       # cell or point
                               breaks=5,
                               len.fun=flow.lens)   #simple.dist.to.outlet)
{
  outlets=NULL      # (not currently used) if routing to multiple basins these are the outlet cells for each

  chans <- chans[[1]]
  if(!is.null(chans))
  {
    # just riprian areas
    if(!is(chans, "Raster"))
    {
      stop("Raster required")
    }
    drn.cells <- which(chans[]>0)
  }
  else
  {
    # use everything
    drn.cells <- which(!is.na(dem[]))
  }
  
  message("Calculating flow distances...")
  
  # calculate flowlengths to outlet
  lens <- do.call(len.fun, list(dem=dem, src=drn.cells,  outlet=outlet))
  
  lens.riv <- dem -dem  + lens
  
  # bin the length into the given number of classes
  len.bin <- cut(lens.riv, breaks=breaks)[]
  
  sel <- which(!is.na(len.bin))
  # two column matrix of reach length cats and reach ids
  len.tab <- data.frame(reach=len.bin[sel], len=lens.riv[sel])
  # mean flow distances for each of the classes
  mean.lens <- sapply(split(len.tab, as.factor(len.tab$reach)), function(tab){mean(tab$len)})
  ftab <- tabulate(len.bin[sel], breaks)   #length(rlens)-1)  #, len=lens.riv[sel])
  props <- ftab/sum(ftab)
  
  routing <- data.frame("flow.len"=round(mean.lens), "prop"=round(props,2))
  # just the default
  if(length(outlets)==0)
  {
    return(routing)
  }
}


build_log_ovf_routing_table <- function(flow_dists,
                                    nbreaks=15, 
                                    dp=4,
                                    chans=NULL)
{
  flow_dists <- flow_dists + 0.1

  icell <- which(!is.na(flow_dists[]))
  if(!is.null(chans))
  {
    ichan <- which(chans[[1]][]>0)
    icell <- setdiff(icell, ichan)
  }
  # log flow dists to compress more distant flows  
  log.lens <- range(log(flow_dists[icell]), na.rm=TRUE)

  breaks.log <- seq(log.lens[1], log.lens[2], length.out=nbreaks)

  bins <- tabulate(cut(log(flow_dists[icell]), breaks=breaks.log, labels=FALSE))
  
  breaks <- exp(breaks.log)
  
  # midpoints of the intervals, removes one break
  breaks <- round((breaks[-length(breaks)]+breaks[-1])/2)
  
  if(length(breaks)!=length(bins))
  {
    stop("No. breaks != no.bins,", length(bins), " check overland routing")
  }
  
  #  bins <- c(bins, max(bins))
  # remove the lowest bound of 0
  return(data.frame("flow.len"=breaks, "prop"=round(bins/length(icell), dp)))
  
  
}

# calculate a distance - areal fraction table for the given raster of 
# overland flow distances to the nearest channel
# anything further than max dist is ignored
build_ovf_routing_table <- function(flow_dists,
                                    nbreaks=8, 
                                    seg_dist=100,
                                    dp=4,
                                    max_dist=max(flow_dists[],  na.rm=TRUE),
                                    chans=NULL)
{

  if(!is.null(seg_dist))
  {
    # add a short break at the start to catch flows from next to the channel
    # truncate at the maximum dist
    breaks <- c(-0.1, 
                seq(xres(flow_dists)+0.1, max_dist, seg_dist))

    # flows from outside this area lumped together so routing doesn't take too long 
    breaks <- c(breaks, max(flow_dists[], na.rm=TRUE)+seg_dist)
  }
  else
  {
    # calculate the bins, adding a buffer at either end
    breaks <- seq(0, max(flow_dists[], na.rm=TRUE)*1.1, length.out=nbreaks)
  }
  
  icell <- which(!is.na(flow_dists[]))
  
  if(!is.null(chans))
  {
    ichan <- which(chans[[1]][]>0)
    icell <- setdiff(icell, ichan)
  }
  
  bins <- tabulate(cut(flow_dists[icell], breaks=breaks, labels=FALSE))
  
  # midpoints of the intervals, removes one break
  breaks <- round((breaks[-length(breaks)]+breaks[-1])/2)
  
  if(length(breaks)!=length(bins))
  {
    stop("No. breaks != no.bins,", length(bins), " check overland routing")
  }
  
  iout <- which(breaks > max_dist)
  
  
#  bins <- c(bins, max(bins))
  # remove the lowest bound of 0
  return(data.frame("flow.len"=breaks, "prop"=round(bins/length(icell), dp)))

  
}
