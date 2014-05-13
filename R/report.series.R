report.series <-
function(obs, col=1)
{
  if(!is.zoo(obs)){stop("Time series required")}
  if(ncol(obs)>1)
  {
    return(report.multi.series(obs))
  }
  obs <- obs[,col]
  data.periods <- rle(as.vector(!is.na(obs)))

  starts <- index(obs)[c(1, cumsum(data.periods$lengths)+1)] 
  ends <- index(obs)[cumsum(data.periods$lengths)] 
    df <- NULL
  # run through each period
#  df <- data.frame()
  for(i.len in 1:length(data.periods$lengths))
  {          
    df <- rbind(df, c("start"=format(starts[i.len]), "end"=format(ends[i.len]), 
                      valid=data.periods$values[i.len])) 
    
  } 

  return(data.frame(df))
}
