LineLengths <-
function(lns)
{
	sapply(lns@lines, 
		   function(lns)
		   {
		   	sapply(lns@Lines,
		   		   function(l)
		   		   {
		   		   	
		   		   	LineLength(l)})
		   })	
}
