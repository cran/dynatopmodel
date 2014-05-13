WeightedAverage <-
function(vals, weights)
{
  sum(vals*weights/sum(weights))
}
