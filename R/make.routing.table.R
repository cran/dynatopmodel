make.routing.table <-
function(dem, hru, drn=NULL, reaches=NULL, nbreaks=15)
{	
	reaches <- determine.reaches(dem, drn, reaches)
#	drn.cells <- extract.cells(dem, drn)
	#
	hru <- hru[[1]][]

	# build a reach table based on flow distance to outlet
	lens <- flow.lens(dem)
	lens.riv <- reaches[[2]] - reaches[[2]] + lens
	# bin the lengths into desired number of reach lengths
	len.bin <- cut(lens.riv, breaks=nbreaks)[]
	sel <- which(!is.na(len.bin))
	# two column matrix of reach length cats and 
	len.tab <- data.frame(reach=len.bin[sel], len=lens.riv[sel])
	# calculate mean flow distances for each of the classes
	mean.lens <- sapply(split(len.tab, as.factor(len.tab$reach)), function(tab){mean(tab$len)})
	ftab <- tabulate(len.bin[sel])  #, len=lens.riv[sel])
	props <- ftab/sum(ftab)	
	routing <- data.frame("flow.len"=round(mean.lens), "prop"=signif(props,2))
	return(routing)
	# tabulate classes of cells containing the channel and flow distance ranges
# 	reach.tab <- table(dist=len.bin[sel], hru=hru[sel])
# 	
# 	# frequency table
# 	ftab <- signif(prop.table(reach.tab,2),3)
# 	
# 	# cumulative column sum to be compatible with TOPMODEL?
# 	
# 	# save the distances within each reach category 
# #	row.names(ftab) <- round(mean.lens)
# 	ftab <- cbind("dist"=round(mean.lens), ftab)
# 	return(ftab)	
}
