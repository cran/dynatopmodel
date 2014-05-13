disc.topidx <-
function(dem, riv, nbreaks)
{
  atb<-upslope.area(dem, atb=T)[["atb"]]
  if(!is.null(riv))
  {
    atb[which(riv[]>0)]<-NA  # ignore river cells
  }

  atb.hist <- hist(atb, breaks=nbreaks, freq=F)
  tot <- sum(atb.hist$counts)
  # second col is for proportion occupied - final entry is zero for count of cells > final break
  topidx.frame <- cbind(atb=atb.hist$breaks, prop=c(atb.hist$counts/tot, 0))
  return(topidx.frame)
}
