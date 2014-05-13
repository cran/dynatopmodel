\name{run.gof}
\alias{run.gof}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Quantatitive goodness of fit measures
}
\description{
Return a selection of efficiency measures for simulated vs observed values
}
\usage{
run.gof(qsim, qobs, what = c("NSE", "R2"), digits = 3)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{qsim}{
simulated values: any series that can be cast to a vector
  }
  \item{qobs}{
observed values (same length and units as simulated values)
}
  \item{digits}{
number of decimal places in the results
}
  \item{what}{
A list of character vectors giving the measures to output. Options are:

R2: Coefficient of Determination

rSD: Ratio of Standard Deviations

NSE: Nash-Sutcliffe efficiency

mNSE: Modified Nash-Sutcliffe efficiency

rNSE: Relative Nash-Sutcliffe efficiency 

m: modified Index of Agreement 

d: Index of Agreement

me: Mean Error

mae: Mean Absolute Error

rms: Root Mean Square Error

nrms: Normalized Root Mean Square Error

r: rPearson product-moment correlation coefficient

r.Spearman: Spearman Correlation coefficient

rSD: Ratio of Standard Deviations

cp: Coefficient of Persistence

pbias: Percent Bias

KGE: Kling-Gupta efficiency

bR2: the coef. of determination multiplied by the slope of the linear regression between 'sim' and 'obs'

VE: volumetric efficiency 
}
}

\value{
A list comprising the values of the specifed measures calculated using the observed and simulated values
}
\references{
Boyle, D. P., H. V. Gupta, and S. Sorooshian (2000), Toward Improved Calibration of Hydrologic Models: Combining the Strengths of Manual and Automatic Methods, Water Resour. Res., 36(12), 3663-3674 

Criss, R. E. and Winston, W. E. (2008), Do Nash values have value? Discussion and alternate proposals. Hydrological Processes, 22: 2723-2725. doi: 10.1002/hyp.7072

Gupta, Hoshin V., Harald Kling, Koray K. Yilmaz, Guillermo F. Martinez. Decomposition of the mean squared error and NSE performance criteria: Implications for improving hydrological modelling. Journal of Hydrology, Volume 377, Issues 1-2, 20 October 2009, Pages 80-91. DOI: 10.1016/j.jhydrol.2009.08.003. ISSN 0022-1694 

Mauricio Zambrano-Bigiarini (2014). hydroGOF: Goodness-of-fit functions for comparison of simulated and observed hydrological time series. R package version 0.3-8. http://CRAN.R-project.org/package=hydroGOF
  
Yilmaz, K. K., H. V. Gupta, and T. Wagener (2008), A process-based diagnostic approach to model evaluation: Application to the NWS distributed hydrologic model, Water Resour. Res., 44, W09417, doi:10.1029/2007WR006716
}
\author{
Peter Metcalfe, using routines by Mauricio Zambrano-Bigiarini
}

\seealso{
\code{\link{run.sets}}
}
\examples{
\dontrun{
  require(dynatopmod)
  data(mor_demo_run)  
  with(mor.demo.run, run.gof(qsim, qobs))
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
