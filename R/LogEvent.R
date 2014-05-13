LogEvent <-
function(msg, tm=NULL, # time can be actual run time or simulation time
                     warn=F)  #, logout=stderr())  #System.getenv("log"))  # destination
{
  if(.show.message)
  {    
     # browser()
		if(is.null(tm)){tm=Sys.time()}
    msg <- paste(as.character(tm), ": ", msg)
    if(warn)
    {
      warning(msg, immediate.=T)
    }
    else
    {
      message(msg)
    }
  
  }
 # message(msg)
}
