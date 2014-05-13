DisplayObservedFlows <-
function (qobs, start, end, xlim, ylim)
{
  if(!is.null(qobs))
  {
    # pick up labels from the series names      
    # observed discharges - can be a number of series
    nobs <- ncol(as.matrix(qobs))
    qobs <- GetTimeSeriesInputRange(qobs, start, end, verbose=FALSE)  # sum(groups$area) * 1e9
    cols <- get.qobs.cols(qobs)
    for(iobs in 1:nobs)
    {     
      plot.zoo(qobs[,iobs], main="", col=cols[iobs], 
               cex.axis=0.75,
               cex.lab=0.5,
               ylab="Specific discharge (mm/hr)",
               xlim=xlim,
               ylim=ylim,
               lwd=1, lty=2, ann=FALSE, 
               #axes=FALSE, 
               xaxt="n",
               yaxt="n",
               bty="n")  # , ylim=c(0,qrmax)
      par(new=T)      
    }    
  }
}
