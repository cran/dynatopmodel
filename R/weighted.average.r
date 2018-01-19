weighted.mean <- function(vals, weights)
{
  sum(vals*weights/sum(weights))
}
