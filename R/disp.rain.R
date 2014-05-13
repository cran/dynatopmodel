disp.rain <-
function(rain, disp.par=def.disp.par(), fact=4)
{
  # inverted plot of rainfall
  # reversed limits, plus a buffer
  ylim2 <- c(max(c(fact*rain),na.rm=TRUE),0)  
  
  plot.zoo(rain, ylim=ylim2, type="h", plot.type="single",
           ylab="",  
           lwd=2,  
           xaxt="n",
           yaxt="n", 
           #cex.main=1,
           bty="n",
           col=rain.cols(rain),
           xlab="")
           #cex.axis=cex.axis,
           #cex.lab=cex.axis)
           
  # right hand axis - precipitation and pe
  at <- pretty(range(rain))
  axis(side=4, at=at)#, labels=at)    
  
  # grid lines
  abline(h=at, lty=3, col="gray")
   mtext("Precipitation (mm/hr)", las=3, side=4, padj=2, line=1, 
         cex=disp.par$"cex.axis")
           #cex.axis        
  
  
}
