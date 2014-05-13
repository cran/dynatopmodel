\name{run.proj}
\alias{run.proj}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Run a Dynamic TOPMODEL simulation
}
\description{
Runs a single discretisation associated with the given project, dealing with aggregation of time series input to a desired time step and checking for missing and invalid input.
}
\usage{
run.proj(proj, idisc = 1, disc = NULL, qobs = NA, rain = NULL,
         start = NULL, end = NULL, disp = F, show.stats = T,
         run.tm = F, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{proj}{
Project wrapper comprising, minimally, rainfall data and at least one discretisation.
}
  \item{idisc}{
  Index of discretisation to run the data against.
}
  \item{disc}{
Run this discretisation in preference to that specified by idisc.
}
  \item{qobs}{
Observed specific discharges, used in preference to any loaded with the project. Useful if running simulation repeatedly and using the previous results as comparisons.
}
  \item{rain}{
Optional rainfall data (m/hr) overwriting project's data
}
  \item{start}{
Simulation start time. Use project setting if not supplied or infer from supplied input data.
}
  \item{end}{
Simulation end time. Use project setting if not supplied or infer from supplied input data.
}

\item{disp}{
Display the results after run
}
  \item{show.stats}{
Show efficiences at end of run
}
  \item{run.tm}{
Show run duration (s) at end of run.
}

\item{...}{
Any other parameters that be passed to run.dtm
}
}
\details{
Dealing with the input to a simulation can be complicated, particularly if input have differing frequencies. The procedudre conveniently runs the simulation given the data contained in a Dynamic TOPMODEL project wrapper and handles aggregating any time series input to a common frequency and checking for missing or invalid data.
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
\code{\link{disp.run}}
}
\examples{
\dontrun{
# morland demo catchment
data(mor)
run<-run.proj(mor)
disp.run(run)

}

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
