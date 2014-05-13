\name{apply.params}
\alias{apply.params}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Apply parameters to all sets in a project
}
\description{
Apply model parameters to all response units within all or a subset of discretisations within in a project.
}
\usage{
apply.params(proj, params, which = 1:length(proj$disc))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{proj}{
%%     ~~Describe \code{proj} here~~
}
  \item{params}{
List of parameters to apply in form param_name=value
}
  \item{which}{
The indices of the HRU to which to apply the parameters.
}
}
\value{
The updated project
}

\author{
Peter Metcalfe
}

\examples{
  \dontrun{
  require(dynatopmod)
  
  # load the Morland DTC project
  data(mor)
  data(mor.demo.run)
  mor$obs$qobs <- with(mor.demo.run, cbind("Observed"=ts[,"qobs"], "lnt0=11.8"=ts[,"qsim"])) 
  mor$sim.start<-start(mor.demo.run$qobs)
  mor$sim.end<-end(mor.demo.run$qobs)
  # see the effect of reducing the saturated transmissivity
  mor <- apply.params(mor, list(ln_t0=10))
  
  run.proj(mor)
  }
}

