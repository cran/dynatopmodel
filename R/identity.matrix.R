identity.matrix <-
function(n)
{
  mat <- matrix(ncol=n,nrow=n)
  mat[] <- 0
  mat[col(mat)==row(mat)]<-1
  return(mat)
}
