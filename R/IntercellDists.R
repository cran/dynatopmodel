IntercellDists <-
function(dem, from, to)
{
	srcRC <- rowColFromCell(dem, from)
	destRC <- rowColFromCell(dem, to)	
	dRC <- srcRC-destRC	
	dxy <- dRC * c(xres(dem), yres(dem))
	return(sqrt(rowSums(dxy^2)))
}
