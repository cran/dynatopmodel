report.multi.series <-
function(obs)
{
  res <- lapply(1:ncol(obs), function(icol){report.series(obs[,icol])})
  names(res)<- colnames(obs)
  return(res)

}
