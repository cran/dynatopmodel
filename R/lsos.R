lsos <-
function(..., n=10, query=F)
{
	objs <- .ls.objects(..., order.by="Size", decreasing=TRUE, head=TRUE, n=n)
	if(query)
	{
		print(objs)
		for(i in 1:nrow(objs))
		{
			if(readline(paste("Delete ", row.names(objs)[i], "?"))=="y")
			{
				# items listed exist in global environment
				rm(dQuote(row.names(objs)[i]), envir=.GlobalEnv)
		#		cat(obj, " deleted\n")

			}
		}
		# return memory to os immediately
		gc()
	}
	return(objs)
}
