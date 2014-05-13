disp.qsim <-
function(qsim, xlim, ylim, disp.par)
{
  par(new=T) 
  # calculate y limits from maximum of evap, observed flows plotted on LH axis
 # ylim <- c(0, max(c(0.01, ymax, qsim), na.rm=T))  # evap
  
  # simulated discharge (note complete time range displayed)
  plot.zoo(qsim, main="", plot.type="single", 
           xlim=xlim, 
           ylim=ylim, 
         #  cex.axis=1, #cex.axis,
        #   cex.lab=cex.axis,         
           ylab="", #Specific discharge / evap. (mm/hr)",
           col=qsim.cols(qsim), lwd=1, 
           #ann=FALSE, 
           xaxt="n", 
           yaxt="n",
           bty="n", xlab="") 
 
  # horizontal grid lines up to max discharge
  at <- pretty(ylim)#  pretty(par("usr")[3:4])
  at <- at[-length(at)]
  abline(h=at, col="gray", lty=2)    
  axis(2, at=at)
  mtext("Specific discharge / evap. (mm/hr)", las=3, side=2, padj=2, line=4, 
        cex=disp.par$"cex.axis")

}
