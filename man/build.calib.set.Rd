\name{build.calib.set}
\alias{build.calib.set}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Construct Latin hypercube of Dynamic TOPMODEL parameters
}
\description{
Generate mulitiple parameter sets suitable e.g. for calibration
}
\usage{
build.calib.set(groups, params)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{groups}{
table of HRU definitions
}
  \item{params}{
list of parameter values of the form param_name=values. 
}
}
\details{
The function will create a data frame consisting of model parameters containing every permuation of the given parameters. Items generated from params will overwrite the corresponding values found in groups.
}
\value{
A data frame with the columns found in groups but containing every combination of the supplied parameters
}
\references{
A Comparison of Three Methods for Selecting Values of Input Variables in the Analysis of Output from a Computer Code. M. D. McKay, R. J. Beckman and W. J. Conover.Technometrics, Vol. 21, No. 2 (May, 1979), pp. 239-245
}
\author{
Peter Metcalfe
}
\note{
Items in params not found in colnames(groups) will be ignored.
}

\seealso{
\code{\link{run.sets}}
}
\examples{
  \dontrun{
  require(dynatopmod)
  
  # load the Morland DTC project
  data(mor)
  
  # initial parameter values
  hru <- mor$disc[[1]]$groups
  
  # parameter value ranges
  m <-seq(0.007, 0.011, by=0.001)
  ln_t0<-seq(13.2, 15.2, by=0.4)
  
  # explicit values for less sensitive parameters
  td <- c(0.5, 1, 10)
  vchan <- seq(500, 4000, by=500)
  
  # build a calibration set
  calib.set <- build.calib.set(groups=hru, 
                             list(td=td, vchan=vchan, ln_t0=ln_t0, m=m)))

  cat("No. calibration sets = ", nrow(calib.set), "\n")

  res <- run.sets(mor, calib.set=calib.set)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~calibration }
