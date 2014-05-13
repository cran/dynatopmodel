\name{gwy.demo.run}
\alias{gwy.demo.run}
\docType{data}
\title{
Calibration run for Gwy catchment
}
\description{
Results of a calibrated run for the Gwy catchment, upper Wye, Plynlimon research catchments, mid Wales, UK.
}

\details{
\figure{gwyover.jpeg}

The project contains just one discretisation of 9 HRUs obtained by splitting the catchment according to upslope area. Time step = 15 minutes with 5 inner time steps giving effective step of 3 minutes; NSE=0.94. The data and calibration procedure to obtain this result is described in Metcalfe, Beven and Freer (2014).

\figure{fig11.jpeg}
}


\source{
Metcalfe, Beven and Freer (2014). A modelling framework, implemented in R, for analysis of spatial heterogeneity, connectivity and complexity of landscape features and dominant rainfall-runoff characteristics. In preparation for ENVSOFT.
}

\note{
Rainfall and discharge data are under licenced by the Centre for Ecology and Hydrology, UK. For this reason, and to minimise space, only the simulated discharges, potemtial and actual evaportranspiration are included. Please contact the maintainer, Peter Metcalfe <p.metcalfe@lancs.ac.uk>, or Professor Rob Lamb <rob.lamb@jbatrust.org> to arrange accesss to the original data. 
}
\references{

Marc, V., & Robinson, M. (2007). The long-term water balance (1972-2004) of upland forestry and grassland at Plynlimon, mid-Wales. Hydrology and Earth System Sciences, 11(1), 44-60.

Newson, A. J. (1976). Some aspects of the rainfall of Plynlimon, mid-Wales. Wallingford: Institute of Hydrology.

Newson, M. D. (1976). The physiography, deposits and vegetation of the Plynlimon catchments.(A synthesis of published work and initial findings). Wallingford: Institute of Hydrology.

Newson, M. D., & Gilman, K. (Eds.). (1991). Plynlimon research: the first two decades (p. 188). Wallingford: Institute of Hydrology.

}
\examples{
\dontrun{
require(dynatopmodel)
data(gwy_demo_run)
# plot all series: simulated discharges, pe and ae
plot.zoo(gwy.demo.run)
}
}
\keyword{~Gwy}
\keyword{~Calibration}