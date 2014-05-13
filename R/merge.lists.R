merge.lists <-
function (list1, list2) 
{
	if(length(list2)==0){return(list1)}
	if(length(list1)==0){return(list2)}
  allNames <- unique(c(names(list1), names(list2)))
  merged <- list1 # we will copy over/replace values from list2 as necessary
  for (x in allNames) {
    # convenience
    a <- list1[[x]]
    b <- list2[[x]]
    if (is.null(a)) {
      # only exists in list2, copy over
      merged[[x]] <- b
    } else if (is.list(a) && is.list(b)) {
      # recurse
      merged[[x]] <- merge.lists(a, b)
    } else if (!is.null(b)) {
      # replace the list1 value with the list2 value (if it exists)
      merged[[x]] <- b
    }
  }
  return(merged)
}
