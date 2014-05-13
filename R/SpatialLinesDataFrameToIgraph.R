SpatialLinesDataFrameToIgraph <-
function(my.network)
{
	c<-t(sapply(unlist(coordinates(my.network),recursive=FALSE),FUN=function 
				(x) cbind(x[1,],x[nrow(x),]))) 
	c<-as.data.frame(t(c)) 
	names(c)<-c("From.x","From.y","To.x","To.y") 
	
	# Then complete the vertex table 
	
	n2 <- cbind(my.network,c) 
	v<-unique(rbind( 
		data.frame(X=n2$From.x,Y=n2$From.y),data.frame(X=n2$To.x,Y=n2$To.y))) 
	v$ID <- 1:nrow(v) 
	
	# Then match back to the original network coordinates to assign vertex 
	# IDs to each feature end point 
	
	n3<-merge(n2,data.frame(From=v$ID,From.x=v$X,From.y=v$Y),by=c("From.x"," 
																  From.y")) 
	n4<-merge(n3,data.frame(To=v$ID,To.x=v$X,To.y=v$Y),by=c("To.x","To.y")) 
	
	# Then make an igraph 
	# remember igraph indices are 0-based (although if you don't, you'll 
	# just end up with one unconnected vertex labeled 0) 

	library(igraph) 
	edge.list <- 
		cbind(n4[,c("From","To")],n4[,grep("^(From|To)$",names(n4),invert=TRUE)] 
		) 
	edge.list$From <- edge.list$From-1    # 0-based conversion 
	edge.list$To <- edge.list$To-1 
	g <- graph.data.frame(edge.list,directed=FALSE) # All the shapefile 
	#] attributes are now edge attributes 
	return(g)
}
