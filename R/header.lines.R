header.lines <-
function(fn, sep, max.n=2, skip=0)
{
  res <- -1  # default value inicates no input
  lines <- as.list(readLines(fn, max.n+skip))
  lines <- lines[(skip+1):(max.n+skip)]
  lines.vals <- sapply(lines,
    function(l)
    {
      # keep splitting the line
      s <- strsplit(l, split=sep)
      s2 <- sapply(s, strsplit, split=sep)
      return(any(sapply(s2, is.number)))

    }  
  )
  seqs <- rle(lines.vals)
  # lengthg of the first sequence of F in file, indicating a header line comprised of 
  if(!seqs$values[1]){
    return(seqs$lengths[1])
  }
  return(res)
}
