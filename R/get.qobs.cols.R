get.qobs.cols <-
function(qobs)
{
  #browser()
  #nobs <- ncol(qobs)
  cols <- c("green", "red", "purple", "#7FC97F", "#BEAED4", "#FDC086", "#FFFF99", "#386CB0", "#F0027F", "#BF5B17", "#666666")
  return(cols[1:ncol(qobs)])  #[1:nobs])
}
