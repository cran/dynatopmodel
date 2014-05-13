gof.run <-
function(run, pos=1, 
                    s=first(index(run$qsim)), 
                    e=last(index(run$qsim)))
{
  s <- as.POSIXct(s)
  e <- as.POSIXct(e)
  if(!is.null(run$qobs))
  {
    pos <- min(pos, ncol(run$qobs))
    
    res <- run.gof(subset.zoo(run$qsim, s, e), 
                   subset.zoo(run$qobs[,pos], s, e))
    #     res <- run.gof(as.vector(subset.zoo(run$qsim, s, e)), 
    #                    as.vector(subset.zoo(run$qobs[,pos], s, e)))
    #     
    res <- res[c("NSE","d", "PBIAS %", "R2")]
    return(res)
  }  
}
