\name{gr.on}
\alias{gr.on}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Switch on graphical output
}
\description{
Switches on the graphical output of predicted flows and evapotranspiration whilst simulation is running
}
\usage{
gr.on(proj, spatial = F)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{proj}{
Dynamic TOPMODEL project
}
  \item{spatial}{
Switch on spatial output as well
}
}
\value{
The project with updated display parameters
}


\author{
Peter Metcalfe
}
\note{
Spatial output appears in a separate window
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\examples{
 \dontrun{
  require(dynatopmod)
  
  # load the Morland DTC project
  data(mor)
  
  # run with console output only
  mor <- graphics.on(mor)
  run.proj(mor)
  
  
  }
}
