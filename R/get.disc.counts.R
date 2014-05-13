get.disc.counts <-
function(disc)
{
  counts <- sapply(disc, 
                   function(d)
                   {
                     count <- NULL
                     try(count <- nrow(d$groups), silent=T)
                     return(count)
                   }
  )
  return(counts)
}
