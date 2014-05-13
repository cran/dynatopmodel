\name{create.proj}
\alias{create.proj}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Create and / or load Dynamic TOPMODEL project
}
\description{
The function attempts to load a data wrapper (project) for a Dynamic TOPMODEL catchment analysis from the specified set of directories, and returns the newly created project.
}
\usage{
create.proj(data.dir, id = data.dir, cuts = NULL, area.thresh = 1,
                 chan.width = 1, disc.dir = file.path(data.dir,
                 "disc"), obs = NULL, obs.dir = fp(data.dir, "obs"),
                 ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data.dir}{
  Location of catchment files.
%%     ~~Describe \code{data.dir} here~~
}
  \item{id}{
A character vector identify the project.
}
  \item{cuts}{
If this is supplied a new discretisation with the supplied number and type of cuts is created and added to the project loaded.
}
  \item{area.thresh}{
  Minimum size (percent) for HRU groupings
}
  \item{chan.width}{
Channel width (m)
}
  \item{disc.dir}{
Root directory in which to locate existing and place new catchment discretisations. Defaults to a subdirectory named "disc" of the project directory.
}
  \item{obs}{
Any observations already loaded
}
  \item{obs.dir}{
A directory containing rainfall, discharge and potential evapotranspiration data. These should be in ASCII tab-delimited format and the first column should contain the time of the observation in a POSIX compatible format. If observations could not be loaded this operation gives a warning but the function continues. See load.obs for reading individual data sets.
}
  \item{\dots}{
Any other attributes that should be added to the project on initialisation.
}
}
\details{
A minimal catchment project should contain an elevation raster in GEOTiff format named "dem.tif". Any other rasters with the same extent, resolution and coordinate system located in the specified directory will be assummed to hold relevant landscape data and be added to the layers of a multiband raster named "catch" and added to the project. This attribute may then be passed to disc.catch in order to discretise the catchment according to values found one or more of its layers.
Any vector data held as ESRI shape files (*.shp) located in the project directory will also be loaded and added as elements of the project. One of these should be named drn and define the channel network. If this is not located then a threshold contributing upslope area will be used to infer the channel locations, the default being the 90th percentile of the log(log) of these values.
}
\value{
A project wrapper suitable to be passed to run.proj 
\item{obs}{Rainfall, pe and observed discharge data}
\item{disc}{List of discretisations loaded from the "disc" dir. See add.disc and disc.catch for details of the structure of discretisations. }
%% ...
}

\author{
Peter Metcalfe <p.metcalfe@lancs.ac.uk>
}
\note{
 Note that, in contrast to convention on Windows platforms, the UNIX forward slash is used as path separator .
}

\seealso{
\code{\link{write.proj}}
\code{\link{add.disc}}
\code{\link{disc.catch}}
\code{\link{run.proj}}
}
\examples{
\dontrun{
  require(dynatopmod)
  # Load data for morland catchment
  data(mor)
  # write to a new temporary directory
  dn <- file.path(Sys.getenv("TMP"), "morland")
  write.proj(mor, dn)
  # examine contents
  dir(dn)  
  
  # reload the project, adding a new discretisation
  new.proj <- create.proj(dn, cuts=c(a=20))
  
  # show
  dev.new()
  plot(new.proj$dem)
  plot(new.proj$drn, add=T, col="blue", lwd=2)
  }
}
