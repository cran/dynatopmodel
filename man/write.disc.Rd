\name{write.disc}
\alias{write.disc}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Write files from a catchment discretisation to a disk location
}
\description{
This can be used to save the files for the given discretisation to a new location or to overwrite the files alreday associated. This may be necessary due to e.g. a rebuild due to a changed channel width
}
\usage{
write.disc(disc, dn = disc$dir)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{disc}{
Discretisation
}
  \item{dn}{
Optional. New disk location for files. Will be created if not extant.
}
}

\value{
The discretisation with its attributes updated to reflect its new location.
}
\author{
Peter Metcalfe <p.metcalfe@lancs.ac.uk>
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{rebuild.disc}}
}


\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
