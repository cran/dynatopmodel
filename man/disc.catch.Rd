\name{disc.catch}
\alias{disc.catch}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Discretisation from a multi-band raster of catchment landscape layers
}
\description{
Discretise the catchment using the given landscape layers and a cuts definition
}
\usage{
disc.catch(dem, catch, cuts = NULL, drn = NULL, reaches = NULL,
                 cellprops = NULL, chan.width = NULL, area.thresh =
                 NULL, groups = NULL, routing = NULL, w = NULL, cm =
                 NULL, build.polys = F, ...)
}
\arguments{
  \item{dem}{
Elevation raster. Cells sizes must be expressed in m and be equal in x and y directions. The DEM should use a projected coordinate system. This will be used to construct the flow distribution matrix w.
}
\item{catch}{
Multiband raster comprising catchment landscape layers such as soil classifications, vegetation cover and land use suitable for categorising the catchment area into distinct units (a discretisation). For each list item in cuts there should be a layer with the corresponding name. 
}
  \item{cuts}{
A list of cuts of the form layer_name=number. Each name should correspond to a layer name in the catchment raster. 
}
  \item{reaches}{
Raster containing channel reach locations, of the same dimensions and resolution of the DEM and other catchment layers. The reaches should be numbered sequentially and any areas not containing part of the channel should be NA. If a second band is supplied with values 0-1 then this is taken to be the proportion of the corresponding non-zero cell occuppied by the channel. If this layer is not present then the proportion is infered from the channel width as 

p = max(1, channel.width/xres(dem)

}
\item{cellprops}{
Not used.
}
  \item{area.thresh}{
Minimum area for response units, expressed as a percentage of the catchment plan area.
}
  \item{drn}{
Optional vector digital river network in ESRI Shapefile format. Should use a projected system and have the same CRS as the DEM and other rasters.
}
  \item{chan.width}{
Channel width in m. 
}
  \item{routing}{
Optionally supply a routing table if one available. Otherwise it will be calculated
}
  \item{cm}{
Optional HRU spatial distribution raster. Otherwise the calculated raster will be used.
}
  \item{groups}{
Optionally supply a HRU definition table. Otherwsie this will calculated from the other data.
}
  \item{build.polys}{
Build a shapefile comprising HRU locations and DRN (time-consuming)?

}
\item{...}{Other arguments not matching any of the abvove wil be treated as part of the cuts definition}
  \item{w}{
Supply a flux distribution matrix manually to prevent this being recalculated. If its dimensions conflict with the number of HRUs in the groups table then it will be rebuilt notwithstanding the value supplied in w. 

}}
\details{
The DEM is typically provided from the parent project, as each discretisation should use the same elevation data. Channel locations may vary between duscretisations depending on the source, level of detail and channel width applied. If the reaches raster is not supplied, the function first tries to create one using the DRN and specified channel width, if that is also missing a threshold contributing area is applied to identify the channel.

The cuts are applied in the order in which they appear in the list.
}
\value{
\item{w }{Flux distribution matrix. A (nh+nc) x (nh =nc) matrix defining the downslope flux distributions between groups, between land and the channel, and between channel reaches, where nh is the number of land discretisations identified by applying cuts to the catchment layers and nc the numbbr of channel reaches defined. The nth row gives the proportions of flow out of HRU #n to other response units and the channel. Row sums should thus always add to 1. The m th column gives the proportion of flow from the other response units into the m th group.}
\item{groups }{ A data frame comprising the names, size and parameters of each channel and response units}
\item{hru }{ Multi-band raster comprising: the original rasters that the specifed cuts were applied to produce the discretisation; the channel network;the resultant response unit locations}
\item{ichan }{ Integer index of the channel response units within the groups table. May be changed in future so that the land units and the channels are returned separately}
}


\author{
Peter Metcalfe <p.metcalfe@lancs.ac.uk>
}
\note{
The channel location raster may extend beyond the bounds of the DEM and catchment layers. If this is the case then it will be cropped to their extent.
}

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
\code{\link{disc.from.dir}}
}
\examples{
\dontrun{
# load the Morland Eden DTC mitigation catchment
require(dynatopmod.dtm)
data(mor)
attach(mor)

# discretise by slope and and upslope area
slope <- terrain(dem, opt="slope", unit="degrees")
a <- upslope.area(dem)
# create a two-band catchment raster
catch <- stack(list("a"=a, "slope"=slope))
disc <- disc.catch(dem, catch, cuts=c(a=4,slope=3), drn=drn)
# show  results
dev.new()
plot(disc$hru)
}
}