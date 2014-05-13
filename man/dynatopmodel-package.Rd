\name{dynatopmodel-package}
\alias{dynatopmodel-package}
\alias{dynatopmodel}
\docType{package}
\title{
Implementation of the Dynamic TOPMODEL hydrological model
}
\description{
A native R implementation and enhancement of Dynamic TOPMODEL, Beven and Freers' (2001) extension to the semi-distributed hydrological model TOPMODEL. Includes digital terrain analysis for discretisation of catchments by topographic indexes and other geo-referenced landscape layers.


TOPMODEL (Beven & Kirkby, 1979) is a well-established and widely used hydrological model that implements a spatial aggregation strategy ("discretisation") in order to reduce its computational demands. Beven and Freer (2001) introduced a "dynamic variant that addressed some of the limitations of the original TOPMODEL but which retained its computational and parametric efficiency. In particular, the original assumption of a quasi-steady water table was replaced by time-dependent kinematic routing within hydrological similar areas identified by the discretisation procedure.  

\figure{disc.jpeg}
  
Figure 1: Aggregation of landscape layers into a catchment discretisation and its associated data structures (Metcalfe, Beven and Freer, 2014) 

The new formulation allows a more flexible discretisation, variable upslope drainage areas and spatially variable physical properties, allowing the introduction of any type of landscape data to identify the aggregated areas, referred to as hydrological response units (HRUs). It retains the core dynamics of the FORTRAN implementation but makes use of data storage and vectorisation features of the R language to allow efficient scaling of the problem domain. The preprocessing routines supplied incorporate handling of geo-referenced spatial data to allows it to integrate with modern GIS through industry-standard file formats such as GEOTiff and ESRI Shapefiles, see figure 1 and the documentation for disc.catch, disc.from.dir and add.disc

}

\details{
\tabular{ll}{
Package: \tab dynatopmodel\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2014-04-28\cr
Depends: \tab 
raster,
xts,\cr
Imports: \tab 
fields,
rgeos,
maptools,
rgdal,
sp,
spam,
hydroGOF,
topmodel,
intervals,
tools\cr
Suggests: \tab 
rgl,
igraph\cr
License: \tab GPL (>= 2)\cr
}

}
\author{
Peter Metcalfe, based on Fortran code by Keith Beven and Jim Freer

Maintainer: Peter Metcalfe <p.metcalfe@lancaster.ac.uk>

}

\note{ The package has arisen from PhD research undertaken as part of a  project funded by the JBA Trust (http://www.jbatrust.org). The Trust is a not-for-profit company to support and promote scientific research, education and training in the fields of environmental risks and resources. Its core funding comes from profits made by the JBA Group. However its objectives are non-commercial and it is independently managed and governed by a board of Trustees.

Due to size limitations just one, stripped-down, catchment project and discretsiation can be supplied with this package. Please contact the author if you would like to arrange access to the full data and examples of other discretisations and catchments.

JBA Trust intend to develop a detailed case study of Dynamic TOMODEL applied to flood and water quality modelling for a medium sized catchment in the UK. Please send Rob Lamb <rob.lamb@jbatrust.org> an e-mail if you are interested in collaborating on this study and / or gaining access to the data. 

Thanks go to Professor Keith Beven at Lancaster Environment Centre,  Dr Barry Hankin at JBA and Professor Rob Lamb at the JBA Trust for their continued supervision and support. 
}

\references{
Beven, K. J. and M. J. Kirkby (1979). A physically based variable contributing area model of basin hydrology. Hydrol. Sci. Bull 24(1): 43-69.

Beven, K. J. and J. Freer (2001). A Dynamic TOPMODEL. Hydrological Processes 15(10): 1993-2011.

Buytaert, W. (2011). topmodel: Implementation of the hydrological model TOPMODEL in R.

Metcalfe, Beven and Freer (2014). A modelling framework, implemented in R, for analysis of spatial heterogeneity, connectivity and complexity of landscape features and dominant rainfall-runoff characteristics. In preparation for ENVSOFT.
}
