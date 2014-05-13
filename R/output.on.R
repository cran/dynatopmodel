output.on <-
function(proj)
{
  proj$disp.par$text <- stdout()
  return(proj)
}
