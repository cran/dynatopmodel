RunSummary <-
function(groups, stores, storage.in, ichan, qsim, qobs, start.time, 
                       rain, ae=0, text.out)
                     #  sat.ex =0, 
                    # qbf.ex = 0, rain=0,
                    #   ae=0)
{
  if(!is.null(text.out)){return()}
  run.time <- difftime(Sys.time(), start.time, units="auto")
  # depending on length of run could be mins, secs (or hours!)
  units <- units(run.time)                    
  # difference between net input and net storage gain
  storage.gain <- current.storage(groups, stores, ichan)-storage.in
  wb <- water.balance(groups, stores, storage.in, ichan, qsim,  
                       rain, ae)
  
  #sum(rain-ae)-sum(qsim)-storage.gain
  
  LogEvent(paste("Run completed in ", round(run.time), units))
  # water balance etc
  cat("Total river discharge      = ", round(sum(qsim),2), "m\n")
#  cat("   saturated excess        = ", round(sum(sat.ex),3), "m)\n")
#  cat("   base flow excess        = ", round(sum(qbf.ex),3), "m)\n")  
  cat("Actual  evapotranspiration = ", round(sum(ae),2), "m\n") 
  cat("Total rain input           = ", round(sum(rain),2), "m\n") 
  cat("Net storage gain           = ", round(sum(storage.gain),2), "m\n") 

  cat("Water balance              = ", round(wb,2), "m\n")
 # eff <- round(RSquared(qr, qobs), 2)
#  message(paste("R²=", eff))
}
