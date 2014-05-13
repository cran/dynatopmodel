MatrixToArrayIndex <-
function(mat,row,col)
{
	if(!is.matrix(mat))
	{ stop("Non-matrix supplied to MatrixToArrayIndex function")}
	
	return(row + (col-1)*nrow(mat))
}
