\name{add.disc}
\alias{add.disc}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Add new or replace an existing discretisation
}
\description{
This function will create a new discretisation according to the catchment layers present in proj, and either append it to the disc list or replace an existing item.

}
\usage{
add.disc(proj, cuts = NULL, i.disc = 0, rebuild = F, chan.width= 2, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{proj}{
Dynamic TOPMODEL project wrapper
}
  \item{cuts}{
cuts definition, a named list
}
  \item{i.disc}{
  index of discretisation in the disc list of proj that should be replaced by the new values.
}

  \item{chan.width}{
  Channel width (m)
}
  \item{rebuild}{
If a discretisation with the same parameters exists, should its files be overwritten?
}

  \item{\dots}{
If any additional parameters match the names in proj$catch then they will be treated as additional cuts. Otherwise they will be passed as values that can be supplied to disc.catch.
}
}
\value{
If i.disc is 0 or NULL, the new discretisation. This can then be added or inserted into the project. Otherwise the project with the discretisation inserted at the given index in the list of discretisations.
}

\author{
Peter Metcalfe
}
\note{
If i.disc is outside the range of the list index then the new discretisation will be appended.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\examples{
\dontrun{
# load the Morland Eden DTC test project
require(dynatopmod)
data(mor)

# discretise by slope and aspect
slope <- terrain(mor$dem, opt="slope", unit="degrees")
a <- upslope.area(mor$dem)

# new catchment stack
mor$catch <- stack(list("a"=a, "slope"=slope))
# new discretisation 
disc <- add.disc(mor, a=4, slope=3)
plot(mor$disc$hru)
}
}