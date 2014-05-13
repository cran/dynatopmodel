VectProd <-
function(x1, x2)
{
	x1 <- GetCoords(x1)
	x2 <- GetCoords(x2)
	dx1 <- x1[2,]-x1[1,]
	dx2 <- x2[2,]-x2[1,]
	
	return(sum(dx1*dx2)) # /LineLength(x1)*LineLength(x2))
	
}
