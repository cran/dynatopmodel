check.obs <-
function(input, msg=F)
{

#   # return number of missing values
  return(length(which(is.na(input))))
}
