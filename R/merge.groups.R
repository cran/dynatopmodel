merge.groups <-
function(cm, thresh, ids=unique(cm))
{
 # ids <- unique(cm)
  # id mapping 
  map <- data.frame()
  i <- 1
  ntot <- length(which(!is.na(cm[]))) 
  while(i <= length(ids))
  {
    id <- ids[i]
    tot <- length(which(cm[]==id))
    j<-1
    map <- rbind(map, c(id, id))
    while((i+j)<=length(ids) & (tot/ntot)<thresh)
    {
      idj <- ids[i+j]
      tot <- tot + length(which(cm[]==idj))
      map <- rbind(map, c(idj, id))
      j<-j+1
    }
    # keep collecting until reaching threshold    
    i<- i+j
  }    
  ids <- rev(ids)  
  return(subs(cm, map))   
}
