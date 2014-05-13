plot.resp <-
function(res, eff="NSE", var="m", ...)
{
  par(mgp=c(2,1,0))
  par(family="serif")
  par("mar"=c(4,3.5,3.5,3.5))

  rsr.col<- "brown"
  
  plot(res[,c(var, eff)], col="black", pch=20, cex=0.5, yaxt="ny")  #, ylim=c(0.85,0.89))
    
  axis(2, at=pretty(range(round(res[,eff],2), na.rm=T)))
  grid()
  # determine maximum bounding line (pareto front)
  u.bound <- sapply(sort(unique(res[,var])),
    function(x)
    {
      # values at this x
      vals <- res[which(res[,var]==x),]    
      max.eff <- max(vals[,eff], na.rm=T)  
      return(c(x, max.eff))
       
    }
    )
  u.bound <- t(u.bound)
  lines(u.bound, lwd=1, lty=1, col="slategray")
  max.eff <- u.bound[which.max(u.bound[,2]),2] 
  max.eff.val <- u.bound[which.max(u.bound[,2]),1] 
  abline(v=max.eff.val, col="red")
  abline(h=max.eff, col="red")
  mtext(side=4, at=max.eff, signif(max.eff,2), cex=0.75, line=1)
  
  par(las=2)
  mtext(side=3, at=max.eff.val, signif(max.eff.val,3), cex=0.75, line=1)
  
 # par(new=T)
#  plot(res[,c(var, "RSR")], col=rsr.col, pch=".", cex=3, axes=F, ann=F)
#  axis(side=4)
  
 # mtext("RSR", side=4, line=2, col=rsr.col)

}
