normalise.rows <-
function(w)
{
  if(!is.matrix(w)){warning("matrix input expected to normalise.rows"); return(w)}
  if(any(rowSums(w)!=1))
  {
    cat("Normalising rows...\n")  
    w <-apply(w, MARGIN=1, FUN=function(x)
    { x/sum(x, na.rm=T)}
    )
    #  w[1:nrow(w),]<-w[1:nrow(w),]/rowSums(w)
    # transpose to get in rightg row. col order
    w <- t(w)   
  }
  # need to transpose to get in right row-col order
  return(w)
}
