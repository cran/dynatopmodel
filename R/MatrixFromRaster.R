MatrixFromRaster <-
function(ras)
{
	mat<- matrix(nrow=ras@ncols, ncol=ras@nrows, getValues(ras))
	# invert vertically
	mat<-mat[,ncol(mat):1] 
	return(mat)    
}
