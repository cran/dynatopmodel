dens <-
function(x,n=5){
	## densify a vector
	out = rep(NA,1+(length(x)-1)*(n+1))
	ss = seq(1,length(out),by=(n+1))
	out[ss]=x
	for(s in 1:(length(x)-1)){
		out[(1+ss[s]):(ss[s+1]-1)]=seq(x[s],x[s+1],len=(n+2))[-c(1,n+2)]
	}
	out
}
