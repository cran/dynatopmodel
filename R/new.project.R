new.project <-
function()
{  
  res <- environment()
  res$disp.par <- def.disp.par()
  res$run.par <- def.run.par()
  res$hsu.par<- def.hsu.par()
  res$description = "Dynamic TOPMODEL project file"
  res$run.title <-""
  res$disp.title <- ""  			
  res$notes <- ""
  res$dt<-1
  res$ntt<-1
  res$qt0<-1e-5
  res$qmax<-2/1000
  return(res)
}
