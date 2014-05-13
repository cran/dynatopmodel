open.devices <-
function(n=1, title="", width=8, height=8)
{
  dev.nm <- getOption("device")
  dev.nm.2 <- gsub("X11", "windows" , dev.nm)
  devs <- which(names(dev.list())==dev.nm.2)
  
  # determine number of new windows required by comparing list of open devices with default device opened by call to dev.new
  n.dev <- length(devs)
  n.new <- max(0, n-n.dev)
         
  i<-0
  while(i<n.new)
  {
    # open required number of devices of default type    
    try(dev.new(title=title, width=width,height=height), silent=F)    
    i <- i+1
  }
  id <- NULL
  # return id of nth window of default type
  try(id<-dev.list()[devs][n])
  return(id)
  
}
