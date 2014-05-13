write.obs <-
function(fn, dat, 
                          start = index(dat)[1], 
                          end = index(dat)[nrow(dat)], sep="\t", 
                          which.col=1:ncol(dat), ...)
{
#  if(is.null(start)){start<-}
  tz <- slot(index(dat), "tzone")
  end <- as.POSIXct(end, tz=tz)
  start <- as.POSIXct(start, tz=tz)
  tms<- index(dat)
  sel <- which(tms<=end & tms>=start)
  subdat <- dat[sel, which.col]
 # tms <- index(dat)[sel] 
  write.table(data.frame(subdat), fn, quote=F, sep=sep,...)
  
}
