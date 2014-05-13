clean.up.dirs <-
function(dir, pattern="*.*")
{
	fns <- dir(dir, pattern, recursive=T)
	dupes <- tolower(fns[which(duplicated(tolower(basename(fns))))])
	cat(length(dupes), " duplicated\n")
	for(fn in unique(basename(dupes)))
	{
		#cat("Duplicated: ", fn, "\n")
		dupes.fn <- fns[which(tolower(basename(fns)) == tolower(fn))]
	 	ndupes <- length(dupes.fn)
		if(ndupes>1)
		{
			cat("\nDuplicated ", paste(1:ndupes, dupes.fn, "\n", sep=": "), "\n")
			keep.which <- readline("keep which?")
			if(keep.which != "")
			{
				for(i.del in setdiff(1:length(dupes.fn), keep.which))
				{
				cat("Removing ", dupes.fn[i.del])
				file.remove(file.path(dir, dupes.fn[i.del]))
				cat("\n")
				}
			}
		}
	}
	
	# remove empty directories
	dns <- list.dirs(dir)
	# cannot remove non-empty directories
	for(dn in dns)
	{
		if(length(dir(dn, include.dirs=T))==0)
		{
			cat("Removing ", dn, "..\n")
			unlink(dn, recursive=T, force=T)
		}
	}
	
}
