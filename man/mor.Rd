\name{mor}
\alias{mor}
\docType{data}
\title{
Hydrometric and topographic data for the Morland Beck test catchment, Eden DTC.
}
\description{
Elevation, DRN, observed flows and rainfall for the Morland subcatchment of the Eden DTC, UK.
}
\usage{data(mor)}

\details{
The Morland is one of the three 10 sq.km test basins located within the Eden Demonstration Test Catchment (DTC) in North West England. Each have been extensively instrumented and supply both discharge and a range of water quality measurements as described by Owen et al. (2012). Rainfall data are collected at 15 minute intervals from two AWS located in the upper part of the catchment. The catchment contains a "control" and a "mitigation" catchment of approximately equal areas. Spatial information for the flow gauging and automated water quality monitoring sites for the subcatchments and the main outlet at Newby Bridge are included (in the elements "sites" of type SpatialPointsDataFrame)

\figure{morover.jpeg}

There is considerable variability in rainfall across the catchment according to elevation and position, with annual totals ranging from around 650mm in the valley bottoms to >2000mm on the high fell tops. Rainfall totals measured by the Morland TBR were 1172 mm in 2012 and 1033 mm in 2013. Potential and actual evapotranspiration are almost identical at ~ 480mm/yr (Allen et al., 2010). An approximate time series of PE giving an equivalent annual total has been generated using the approx.pe.ts function using a max daily total of 3mm.

}
\source{
%%  ~~ reference to a publication or URL from which the data were obtained ~~
DEM:  PROFILE DTM [TIFF geospatial data], Scale 1:10000, Tiles: ny52se,ny62sw,ny51ne,ny61nw, Updated: November 2009, Ordnance Survey (GB), Using: EDINA Digimap Ordnance Survey Service, <http://edina.ac.uk/digimap>, Downloaded: Sat Oct 20 11:34:15 BST 2012; DRN derived from Strategi [SHAPE geospatial data], Scale 1:250000, Tiles: GB, Updated: January 2014, Ordnance Survey (GB), Using: EDINA Digimap Ordnance Survey Service, <http://digimap.edina.ac.uk>, Downloaded: Fri Apr 11 21:50:06 BST 2014

Discharge and AWS data: Eden DTC (http://www.edendtc.org)
}

\references{
Allen, D. J., Newell, A. J., & Butcher, A. S. (2010). Preliminary review of the geology and hydrogeology of the Eden DTC sub-catchments.

Owen, G. J., Perks, M. T., Benskin, C. M. H., Wilkinson, M. E., Jonczyk, J., & Quinn, P. F. (2012). Monitoring agricultural diffuse pollution through a dense monitoring network in the River Eden Demonstration Test Catchment, Cumbria, UK. Area, 44(4), 443-453.

}
\examples{
\dontrun{
require(dynatopmod)

data(mor)

dev.new()
attach(mor)

par(family="serif")
plot(dem, legend=F)
grid()
contour(dem, add=T, col="brown", nlevels=30)
plot(drn, add=T, col="blue")

# monitoring locations and AWS
plot(sites, add=T, col=rainbow(6), pch=16)
legend(x="bottomright", cex=0.8, legend=sites@data$Site_id, col=rainbow(6), bg="white", pch=16)
compassRose(x="topleft", cex=0.9)
scalebar(d=1000, x=c(356000, 516000), type="bar", cex=0.9, below="m", divs=5)
title(sub="Fig1: Morland Beck, Eden DTC (10.1 sq.km),showing locations of automated weather,\n 
  flow and water quality monitoring sites and approx location of channel network.", 
  cex.sub=0.8, adj=0)
  detach(mor)


}
}

\keyword{datasets}
