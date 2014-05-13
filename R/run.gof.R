run.gof <-
function(qsim, qobs, what=c("NSE", "R2"), digits=3)
{
  qobs <- as.vector(qobs)
  qsim <- as.vector(qsim)
  len <- max(length(qobs), length(qsim))
  
  effs <- hydroGOF::gof(qsim[1:len], qobs[1:len],digits=digits)
  
  logNSE<- NSE(log(qsim[1:len]), log(qobs[1:len])) 
  # ratio of the root mean square error to the standard
  # deviation of measured data (RSR),
  res <- as.list(t(effs))
  names(res)<- rownames(effs)
  
  # add NSE of log og observed and simulated values, this could give a better idea of the match for regions with storm flows  
  res <- c(res, "logNSE"=logNSE)  
  
  return(res[what])
}
