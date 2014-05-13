IntersectionAngle <-
function(x1,x2,unit="degrees")
{
	x1 <- GetCoords(x1)
	x2 <- GetCoords(x2)
	dx1 <- x1[2,]-x1[1,]
	dx2 <- x2[2,]-x2[1,]
	v<-VectProd(x1,x2)
	costheta <- v/LineLength(x1)/LineLength(x2)
	ang <- acos(costheta)
	if(unit=="degrees")
	{ return(180/pi*ang)}
	
	return(ang)
	
}
