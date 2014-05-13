update.output <-
function(output, groups, stores, it, ichan, 
                          items=NULL)
{
  # weighted average of storage deficits
 # output[time, "sd"]<- 
  # record river input flow
 # output[time, "qriv.in"] <- sum(Qchan)
  
  # water balance: rain in, evap and specific discharge out    
#  output[it, "wb"]  <- output[it, "wb"] + as.numeric(current.input(groups, rain[it,], output[it, "ae"], Qr[it,]/catchArea))
#  output[time, "ae"] <- WeightedAverage(ae, groups$area)   
#  storage.deficits[time,]<- stores[-ichan, "sd"]
#  base.flows[time,]<- flows[-ichan, "qbf"]
  for(nm in names(items))
  {
    if(!nm %in% names(output))
    {
      output <- cbind(output, rep(0, nrow(output)))
      names(output)<-c(names(output)[1:(length(names(output))-1)], nm)
    }
    output[it, nm] <- items[[nm]]
           
  }
          
    return(output)   
}
