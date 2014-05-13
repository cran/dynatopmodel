SetFigWindow <-
function(xmn=0, xmx=1, ymn=0, ymx=1)
{
  # set the fg parameter to place a window at the given position expressed in 
  # proportion of the screen. Call with no parameters to reset to default window
  par(fig=c(xmn,xmx,ymn,ymx))
}
