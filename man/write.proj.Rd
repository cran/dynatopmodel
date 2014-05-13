\name{write.proj}
\alias{write.proj}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Write a project contents to disk
}
\description{
Writes a Dynamic TOPMODEL project data to files in a specific disk location 
}
\usage{
write.proj(proj, dir = proj$dir, disc.dir = file.path(dir,"disc"), 
  obs.dir = file.path(dir, "obs"), 
  save.disc = T, save.obs = F)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{proj}{
Dynamic TOPMODEL project, a list.
}
  \item{dir}{
Output location. Will be created recursively if not already extant
}
  \item{save.obs}{
Write any observation to a subdirectory "obs" of the output location
}
  \item{save.disc}{
Save each discretisation to the appropriate location
}
  \item{disc.dir}{
Specify an alternative location for discretisations
}
  \item{obs.dir}{
Alternative location for observations
}
}

\value{
The project with its location (containing in $dir) and those of its discretisations updated.
}

\author{
Peter Metcalfe <p.metcalfe@lancs.ac.uk>
}

\examples{
  \dontrun{
  require(dynatopmod)
  # Load data for morland catchment
  data(mor)
  # write to a new temporary directory
  dn <- file.path(Sys.getenv("TMP"), "morland")
  write.proj(mor, dn)
  # examine contents
  dir(dn)  
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
