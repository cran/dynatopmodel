\name{rebuild.disc}
\alias{rebuild.disc}

\title{
Rebuild an existing discretisation
}
\description{
Rebuilds all the files for the specified discretisation from scratch and / or adds new cut levels
}
\usage{
rebuild.disc(proj, i.disc = 1, disc = NULL, dn = disc$dir, what = "*.dat|*.tif", ...)
}

\arguments{
  \item{proj}{
A Dynamic TOPMODEL project
}
  \item{i.disc}{
The index of the discretisation to rebuild
}
  \item{disc}{
A discretisation to use if i.disc not supplied
}
  \item{dn}{
Where to output new files.
}
  \item{what}{
List of files to delete and rebuild. 
}
  \item{\dots}{
Any parameters not matching the above will be added to the list of cuts to apply to the discretisation
}
}

\value{
The project containing the updated discretisation
}

\author{
Peter Metcalfe <p.metcalfe@lancs.ac.uk>
}

\examples{
\dontrun{
require(dynatopmodel)
data(mor)
# rebuild the test discretisation
rebuild.disc(mor)
# show
dir(mor$disc[[1]]$dir)
}
}