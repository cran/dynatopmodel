\name{gr.off}
\alias{gr.off}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Switch off graphical output for simulation runs.
}
\description{
Switches on the graphical output of predicted flows and evapotranspiration whilst simulation is running
}
\usage{
gr.off(proj, spatial = F)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{proj}{
Dynamic TOPMODEL project
}
  \item{spatial}{
Switch off spatial output as well
}
}
\details{
Graphical display of model runs is useful but can be time-consuming especially for longer runs and / or during calibration. This suppresses the graphical output.
}
\value{
The project with updated display parameters
}

\author{
Peter Metcalfe
}

\examples{
 \dontrun{
  require(dynatopmod)
  
  # load the Morland DTC project
  data(mor)
  
  # run with console output only
  mor <- graphics.off(mor)
  run.proj(mor)
  
  
  }
}


% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
