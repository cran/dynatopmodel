% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/xts_util.r
\name{aggregate_obs}
\alias{aggregate_obs}
\title{Resample observation data at a new time interval}
\usage{
aggregate_obs(obs, dt, is.rate = TRUE)
}
\arguments{
\item{obs}{List of times series (zoo) objects with a POSIXct index.}

\item{dt}{New time interval, hours.}

\item{is.rate}{If TRUE then these are rates i.e m/hr. Otherwise they are absolute values across the interval and are scaled before return by a factor equal to the ratio of the old interval to the new interval.}
}
\value{
The list of observations resampled at the new interval.
}
\description{
Takes a list of time series and resample to a new interval.
}
\details{
Time series of observation data are often of different temporal resolutions, however the input to most hydrological models, as is the case with the Dynamic TOPMODEL, requires those data at the same interval. This provides a method to resample a collection of such data to a single interval.
}
\examples{
# Resample Brompton rainfall and PE data to 15 minute intervals
require(dynatopmodel)
data("brompton")

obs <- aggregate_obs(list("rain"=brompton$rain, "pe"=brompton$pe), dt=15/60)

# check totals for Sept - Oct 2012
sum(obs$rain*15/60, na.rm=TRUE)
sum(brompton$rain, na.rm=TRUE)

}
