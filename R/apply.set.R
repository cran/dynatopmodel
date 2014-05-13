apply.set <-
function(disc, res, which.n, apply.to=1:nrow(proj$groups))
{
  nms <- intersect(names(disc$groups), colnames(res))
  disc$groups[apply.to,nms]<-res[which.n, nms]  
  return(disc)
  
}
