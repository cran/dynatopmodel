\name{upslope.area}
\alias{upslope.area}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Upslope contributing area and wetness index calculation
}
\description{
Determine upslope contributing area based on an elevation raster and optionally compute the topographic wetness index.
}
\usage{
upslope.area(dem, log = T, atb = F, deg = 0.1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dem}{
elevation raster (m), with equal sized cells and a projected coordinate system
}
  \item{log}{
return log of values. 
}
  \item{atb}{
return a two-layer raster comprising both the upslope contributing area and the topographic wetness index ln(a/tan(beta))
}
  \item{deg}{
minimum slope to define a sink (degrees)
}
}
\details{
For each point in the DEM, this calculates the total uphill area per unit contour draining downslope.
}
\value{
A raster of the same extent and resolution as the input with one or two bands: the first the (log) of the upslope contributing area per unit contour length (m^2/m) and, if specified, the topographic wetness index ln(a)/tan(beta).
}
\references{
Quinn, P. F., Beven, K. J., & Lamb, R. (1995). The In (a/tan/beta) index: How to calculate it and how to use it within the Topmodel framework. Hydrological processes, 9(2), 161-182.
}
\author{
Peter Metcalfe and Wouter Buytaert
}
\note{
This is a wrapper to the function implemented in the TOPMODEL package by Wouter Buytaert.
}

\examples{

\dontrun{
require(dynatopmod)
data(mor)
a.atb <- upslope.area(mor$dem, atb=T)
plot(a.atb)
}
}
