qsim.cols <-
function(qsim)
{
  cols <-   rev(c("#00FFFF","#00BFDF","#007FBF","#003F9F","#000080")) #colorRampPalette(c("navy", "cyan"), ncol(qsim))
  
  return(cols[1:ncol(qsim)])
}
