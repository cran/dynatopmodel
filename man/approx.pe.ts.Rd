\name{approx.pe.ts}
\alias{approx.pe.ts}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Estimate potential evapotranspiration
}
\description{
In temperate, mid latitudes the contribution of evapotranspiration to the water budget can be relatively minor. An approximation derived from the annual variation in insolation is therefore acceptable (ref)
}
\usage{
approx.pe.ts(start, end, dt = 1, emin = 0, emax = 10/1000)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{start}{
Start date of simulation in format "YYYY-MM-DD"
}
  \item{end}{
End date of simulated values in format "YYYY-MM-DD"
}
  \item{dt}{
Time interval in hours
}
  \item{emin}{
Minimum daily evapotranspiration (m/day)
}
  \item{emax}{
Maximum daily evapotranspiration (m/day)
}
}
\details{
The code assumes a direct relationship between evapotranspiration and insolation. Relative humidity and wind speed are neglected, and the effect of cloud cover not considered. Evapotranspiration is assumed to be nil outside daylight hours, and to vary sinusoidal from zero at dawn and sunset to a maximum at noon and sum to the required daily total.
}
\value{
A regular time series (xts) with interval dt whose values give the estimated potential evapotranpiration at each time. 
}
\author{
Peter Metcalfe, based on code by Keith Beven
}
\note{
Sunrise and sunset times are for a latitude equivalent to the midlands of the UK, at about 54.5 deg N. Daylight saving time is not taken into account.
}

\examples{
\dontrun{
  require(dynatopmod)

# generate a time series of p.e using an annual daily maximum of 3mm
  pe <- approx.pe.ts("2013-01-01", "2013-12-31", emax=3/1000)
  
  # show a few months
  plot.zoo(pe["2013-05-01::2013-07-31"])
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~evapotranspiration }
