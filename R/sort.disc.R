sort.disc <-
function(disc, ...)
{
  counts <- get.disc.counts(disc)
  return(disc[order(counts, ...)])
}
