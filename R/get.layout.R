get.layout <-
function(disp.par)
{
  #    layout(matrix(c(1,1,1,1,1,1,2,2,3), nrow=3, byrow=TRUE)) 
  res <- matrix(c(1,1,1,1), nrow=4, byrow=TRUE)  #2
  
  show.spatial <-  F #disp.par$"graphics.spatial.show"  # & tm >= start
  
  if(show.spatial)
  {
    if(disp.par$graphics.spatial.window.id<1)
    {
      # layout so that discharges occupy top half, storages etc the bottom      
      res <- matrix(c(1,1,1,2,3,3), nrow=2, byrow=TRUE)
    }
    else
    {
      # layout so that discharges occupy top half, storages etc the bottom      
      res <- matrix(c(1,1,1,1,2,3), nrow=3, byrow=TRUE)
    }
  }
  return(res)
}
