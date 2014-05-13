height.diff <-
function(x)
{
  diff <- x[1] - x[2]
  if(diff > 0){return(diff)}
  else{return(0)}
}
