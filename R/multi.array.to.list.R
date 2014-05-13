multi.array.to.list <-
function(x, nms=NULL, index=3)
{
  # get a list from elements in (third) array index
  ls <- apply(x, MARGIN=index, function(x){list(x)})
  ls <- lapply(ls,
               function(x)
               {
                 x <- x[[1]]
                 x[!is.finite(x)]<-0                            
                 return(spam::as.spam(x))
               }
  )
  
  names(ls) <- nms  
  return(ls)
}
