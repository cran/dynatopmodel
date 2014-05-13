\name{run.dtm}
\alias{run.dtm}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Run a Dynamic TOPMODEL simulation
}
\description{
Run a simulation directly without loading a project.
}
\usage{
run.dtm(groups, weights, rain, qobs = NULL, qt0 = NULL, pe = NULL, 
  dt = 1, ntt = 1, ichan = 1, i.out = ichan[1], 
  vchan = 1000, vof = 100, qmax = NULL, routing = NULL, 
  reaches = NULL, reservoirs = NULL, 
  sim.start = NA, sim.end = NA, 
  disp.par = def.disp.par(), run.par = def.run.par())
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{groups}{
HRU data frame
}
  \item{weights}{
Flux distribution matrix
}
  \item{rain}{
Time series (xts or vector) rainfall input (m/hr)
}
  \item{qobs}{
Optional time series (xts or vector) observed specific discharge (m/hr)
}
  \item{qt0}{
Initial specific discharge (m/hr)
}
  \item{pe}{
Optional time series (xts or vector) of potential evapotranspiration specific discharge (m/hr)

}
  \item{dt}{
Desired time step
}
  \item{ntt}{
Number of inner time steps
}
  \item{ichan}{
Channel identifiers
}
  \item{i.out}{
Channel output reach identifier
}
  \item{vchan}{
Used only if  no values specified in HRU table
}
  \item{vof}{
Not used
}
  \item{qmax}{
Maximum discharge (used for display only)
}
  \item{routing}{
Routing table
}
  \item{reaches}{
Not currently used
}
  \item{reservoirs}{
Not currently used
}
  \item{sim.start}{
Simulation start date-time
}
  \item{sim.end}{
Simulation start date-time
}
  \item{disp.par}{
List of output display parameters
}
  \item{run.par}{
Not used
}
}
\details{
run.proj is a more convenient way of running simulations as it deals with aggregating the input series to the correct intervals
}
\value{
\item{qobs}{ time series of observed specific discharges across simulation period (if supplied)  (m/hr)}
\item{qsim}{ time series of simulated specific discharges (m/hr)}
\item{rain}{ time series of input rainfall (m/hr)}
\item{evap}{ time series of supplied potential and calculated actual evapotranspiration (m/hr)}

\item{fluxes}{
A list of HRU specific fluxes at each time step (all in m/hr): 

$qbf: subsurface output (downslope)

$qin: subsurface input (upslope)

$qof: overland flow

$uz: unsaturated gravity drainage into water table
}

\item{storages}{
A list of HRU storages / deficits (all in m):

$ex: surface excess storage

$srz: root zone storage

$sd: subsurface deficit
}
\item{wb}{water balance (m)}
\item{ovf}{total overland flow (m)}
}


\author{
Peter Metcalfe <p.metcalfe@lancs.ac.uk>
}

\seealso{
\code{\link{run.proj}}
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
