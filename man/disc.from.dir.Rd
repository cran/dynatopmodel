\name{disc.from.dir}
\alias{disc.from.dir}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Create / load catchment discretisation from disk files
}
\description{
The function attempts to locate and load any raster files found the given directory, if it exists, containing landscape layers. The specified cuts and other parameters will then are applied to the elevation raster and the layers located compatible with it, and a catchment discretisation returned. If an output directory is specified then the resulting files will be written to that location.
}
\usage{
disc.from.dir(dn, dem, reaches = NULL, routing = NULL, cuts = NULL,
                 area.thresh = NULL, drn = NULL, chan.width = NULL,
                 rebuild = F, dn.out = dn, catch = NULL, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dn}{
Directory name. Will be created recursivley if not extant.
}
  \item{dem}{
Elevation raster. Cells sizes must be expressed in m and be equal in x and y directions. The DEM should use a projected coordinate system.
}
  \item{reaches}{
Raster containuing channel reach locations. The reaches should be numbered sequentially and any areas not containing part of the channel should be NA. If a second band is supplied with values 0-1 then this is taken to be the proportion of the corresponding non-zero cell occuppied by the channel. If this layer is not present then the proportion is infered from the channel width as p = max(1, channel.width/xres(dem)
}
  \item{cuts}{
A list of cuts of the form layer_name=number. Each name should correspond to a raster file found in the directory, which will be bound together into a single multi-band raster (stack) comrising the discretisation and channel locations.
}
  \item{area.thresh}{
Minimum area for response units, expressed as a percentage of the catchment plan area.
}
  \item{drn}{
Optional vector digital river network in ESRI Shapefile format. Should use a projected system and the same CRS as the DEM and other rasters.
}
  \item{chan.width}{
Channel width in m. 
}
  \item{rebuild}{
Existing files in the directory for a discretisation will be used unless rebuild is set to T. In this case they wil be rebuilt from scratch.
}
  \item{dn.out}{
A directory in which to place any new  discretisation files that result from calling the function. If NULL the files are not saved to disk.
}
  \item{routing}{
Optional precalculated routing table
}
  \item{catch}{
Multiband raster of catchment landscape layers e.g. loaded on project creation
}
\item{...}{Any named parmeters not mayching the above will be treated as part of the cuts definition}
}
\details{
The DEM is typically provided from the parent project, as each discretisation should use the same elevation data. Channel locations may vary between discretisations depending on the source, level of detail and channel width applied. If the reaches raster is not supplied, the function first tries to create one using the DRN and specified channel width, if that is also missing a threshold contributing area is applied to identify the channel.
}
\value{
%%  ~Describe the value returned
A catchment discretisation (list) or NULL if the procedure failed. The structure is described in disc.catch

}

\author{
Peter Metcalfe <p.metcalfe@lancs.ac.uk>
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{disc.catch}}
}
