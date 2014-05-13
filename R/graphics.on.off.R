graphics.on.off <-
function(proj, val=T, spatial=F)
{
  proj$disp.par$graphics.show <- val  
  if(spatial)
  {
    proj$disp.par$graphics.spatial.show <- val  
  }
  return(proj)
}
