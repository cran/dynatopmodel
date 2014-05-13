chunk <-
function(d, n)
{
  split(d, ceiling(seq_along(d)/n))
}
