load.disc.data <-
function(fn, rebuild=F, header=T, sep="\t", data=NULL)
{
	if(!rebuild & file.exists(fn))
	{
		cat("Loading data table ", fn, "...\n")
		try(data <- read.table(file=fn, header=header, sep=sep))
	}
	return(data)
}
