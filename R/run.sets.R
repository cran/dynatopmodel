run.sets <-
function(proj, disc=proj$disc[[1]], calib.set, 
                     apply.to = 1:nrow(disc$groups), 
                     ichan=disc$ichan,
										 fn=NULL)
{
  res<- NULL
	messages.off()
  conn<-stdout()

  dn <- getwd()
  if(!is.null(fn))
  {
  	save.fn <- fp(dirname(fn), paste0("resp.jpg"))
  	dn <- dirname(fn)
  	if(!file.exists(dn)){
  		dir.create(dn, recursive=T)
  	}
  	res <- NULL
  	if(file.exists(fn))
  	{
  		try(res <- read.table(fn, header=F, sep="\t", skip=1))
  	}
    if(is.null(res))
  	{
  		# build an initial set
  		res<-data.frame(t(rep(NA, length(nms()))))
  		res<-res[-1,]  		
    }
    # keep only non-NA columsn
    res <- res[,apply(res, MARGIN=2, FUN=function(col){!any(is.na(col))})]
    if(ncol(res)<length(nms()))
    {
      pos <- which(nms()=="vchan")
      res <- cbind(res[,1:(pos-1)], calib.set$vchan[1], res[,pos:ncol(res)])
    }
    # all names. need to pick these up from calib results. this ensure data is in correct format
    colnames(res) <- nms()	
  	# write back to disk and ensure headings included
  	write.table(res, sep="\t", file=fn, row.names=F, quote=F)
  	conn <- file(fn, open="a")
  	on.exit(close(conn))
  }  
  all <- rbind(calib.set, res[,colnames(calib.set)])
  # build complete set and remove duplicates
  which <- which(!duplicated(all, fromLast=T)[1:nrow(calib.set)])
  # find sets unique to those already run
 # unrun <- setdiff(calib.set, res[,colnames(calib.set)])
  cat("Found ", length(which), " new calibration sets", "\n")
  
  pb <- winProgressBar( "Running calibration...", min=1, max=length(which))
  on.exit(close(pb), add=T)
  i.calib <- 1
  for(i in which)
  {
    lab <- paste0(colnames(calib.set), "=", calib.set[i,], collapse=",")
    setWinProgressBar(pb, value=i.calib, label=lab, title = paste("calibration ", i.calib, " of ", length(which)))
    disc <- apply.set(disc, calib.set, i, apply.to=apply.to)
    
    cm<- signif(as.numeric(disc$groups[2,par.names()]),4)
    print(cm)   
    proj$disp.par$title.main <- lab
    run <- run.proj(proj, disc=disc)

    effs <- run.gof(run$qobs, run$qsim)
    
    run.info <- c(effs, run$wb, sum(run$qsim)*proj$dt, 
                  run$ovf, sum(run$evap[,"ae"])*proj$dt,
                  proj$ntt, proj$dt, proj$vchan, cm)
    run.info <- signif(unlist(run.info),4)
   	cat(paste(run.info, "\t"), "\n", file=conn)
  	res <- rbind(res, run.info)
    i.calib <- i.calib +1 
   	flush(conn)	   
    # save 
#     if(nrow(res) > 50 & i.calib %% save.int == 0)
#     {
#     	jpeg(save.fn, width=1024, height=1024)
#     	results.summary(res)
#     	dev.off()
#     }
  }
#  close(pb)
#  close(conn)
  return(data.frame(res))
}
