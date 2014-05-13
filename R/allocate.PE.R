allocate.PE <-
function(pe, groups, it)
{
	# currently as for rainfall 
	# partition rain equally across groups 
	return(allocate.rain(pe, groups, it))  
}
