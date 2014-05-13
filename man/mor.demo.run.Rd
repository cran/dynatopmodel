\name{mor.demo.run}
\alias{mor.demo.run}
\docType{data}
\title{
Demo run for Morland test catchment
}
\description{
Manually calibrated run for the 10 sq. km test catchment contained within the Eden DTC, NW England. 
}

\details{
This is a run using data for the Newby outlet station and the Tipping Bucket Gauge. 10 HSUs obtained by splitting catchment according to upslope area. Channel width applied = 2m. DRN from topographic analysis using SAGA GIS (http://www.saga-gis.org). NSE using 3 min time step = 0.87.

\figure{morrun.jpeg}

See documentation for the supplied project for more details and references related to this data set.
}

\examples{
\dontrun{
data(mor_demo_run)
disp.run(mor.demo.run)
}
}
\keyword{datasets}
