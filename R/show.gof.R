show.gof <-
function(proj, run)
{
  if(!is.null(proj$obs$qobs))
  {
    qsim<- run$qsim   
    qobs <- run$qobs   
    gof <- run.gof(qsim, qobs[,1], what=c("NSE", "R2", "d"))
    cat("Efficiencies:\n")
    cat(paste(names(gof), "=", gof, collapse="; "), "\n")
  }  
  
}
