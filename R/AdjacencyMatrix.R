AdjacencyMatrix <-
function(conns, 
                            n=max(conns), # max vertexes considered,defaults to max encountered in connection matrix
                            directed=T)
{
  res <- matrix(ncol=n, nrow=n)
  res[]<-0	
  
  # conns a list of from -> to vertex numbers 
  ir <-1
  while(ir < nrow(conns))
  {
    from <- conns[ir,1]
    to <- conns[ir,2]
    res[from, to]<-1
    ir <- ir + 1
  }
  # if undirected make symmetric...
  return(res)
}
