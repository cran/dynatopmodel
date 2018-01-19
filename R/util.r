
# Returns function that gives value below which the supplied proportion of the observation lie
get_percentile_function <- function(qsim, pc=TRUE)
{
  # empirical CDF for discharges
  cdf <- stats::ecdf(as.numeric(qsim))

  qmax <- max(ceiling(qsim))

  # range
  qx <- seq(0, qmax, length.out = 1000)

  # cumulative frequencies
  cumfreqs <- cdf(qx)

  if(pc)
  {
    # function expecting a percentage value
 #   cumfreqs<- cumfreqs*100
  }

  # percentile function gives value below which the given proportion of discharges lie
  fn.pcntl <- stats::approxfun(cumfreqs, qx)

  return(fn.pcntl)
}

###############################################################################
# general purpose utilities
###############################################################################
# x object name
# @param val value to set if object is unassigned
# @param if.null Should value be set if values exists but is NULL
set.if.empty <- function(x, val, if.null=TRUE)
{
	if(!exists(x, envir=parent.frame()) || (if.null & is.null(get(x, envir=parent.frame()))))
	{
		# assign the value in the calling environment
		assign(x, val, envir=parent.frame())
	}

}

# return the object from the calling  environment or from the given
# filename is supplied. If loaded from file then check that the object
# matches the supplied name
locate.object <- function(obj.nm,
                          check.null=FALSE,
                          ignore.exists=TRUE,
                          ignore=FALSE,
                          fn=NULL)
{
  if(ignore)
  {
    # always return NULL so that any existing value is ignored
    return(NULL)
  }
  obj.exists <- exists.not.null(obj.nm, check.file = FALSE)
  if(obj.exists & !ignore.exists)
  {
    obj <- get(obj.nm)
    return(obj)
  }
  else if(!is.null(fn) & file.exists(fn))
  {
    obj.loaded <- load(fn, verbose=TRUE)
    # load returns the name of the object loaded - check that this
    if(!obj.loaded==obj.nm)
    {
      warning("Object loaded from", fn, " is ", obj.loaded,
              "required object is ", obj.nm)
    }
    return(get(obj.nm))
  }

  return(NULL)
}

# check that the named object exists and is not NULL
# if check.file is TRUE then checkk to see if the a file
exists.not.null <- function(obj.name, check.file=TRUE, warn=NULL)
{
  res <- FALSE
  # calling frame / environment, up one level in calling stack, must be at least one
  p.env <- sys.frame(-1)
  # treat the argument as a charcater object name and look in the calling frame
  if(exists(obj.name, where=p.env))
  {
    obj <- get(obj.name, p.env)
    if(!is.null(obj))
    {
      res <- ifelse(check.file,
                    file.exists(obj),
                    TRUE)

    }
  }
  if(!res & !is.null(warn))
  {
    warning(warn)
  }
  return(res)
}



# determine if all elements in a vector are unique
is.unique <- function(x)
{
   return(length(unique(x))==length(x))
}

# determine if running locally by looking at the operating system
is.local.machine <- function()
{
	if(Sys.getenv("HOSTNAME") == "")
	{
		return(TRUE)
	}

	return(FALSE)

}

#' Mode - modal value
#'
# return the modal value of a vector
#' @param x Numeric. A vector of numerical values whose mode is wanted
#' @return The modal value
#' @note Capitalisation is to distinguish method name from base::mode
Mode <- function(x) {
	ux <- unique(x)
	ux[which.max(tabulate(match(x, ux)))]
}

# text progress bar that  appears only during intercative sesions
get.pb <- function(...)
{
	pb <- NULL
	if(interactive())
	{
		pb <- txtProgressBar(..., style=3)
		#	on.exit(close(pb))
	}
	return(pb)
}

update.pb <- function(pb, ...)
{
	if(interactive() & !is.null(pb))
	{
		setTxtProgressBar(pb, ...)
	}
}

# take a delimited string containing parameters and
# if of form "name=value" add lits elemet with that name
# if of form "value" just add a list element at that position
parse_key_vals<- function(str, sep="_", equals="=", allow_empties=FALSE)
{
  keyvals <- strsplit(str, split=sep)[[1]]
  nms <- NULL
  vals <- NULL
  if(length(keyvals)==0)
  {
    warning("Couldn't locate any arguments in string ", str)
    return(NULL)
  }
  for(i in 1:length(keyvals))
  {
    keyval <- strsplit(keyvals[i], equals)[[1]]

    if(length(keyval)==1)
    {
      if(!allow_empties)
      {
        warning("invalid key-value ", keyval)
        return(NULL)
      }
      nm <- as.character(i)
      val <- keyval
    }
    else
    {
      nm <- keyval[1]
      val <- keyval[2]
    }

    numval <- as.numeric(val)
    if(is.finite(numval))
    {
      val <- numval
    }
    nms <- c(nms, nm)
    vals <- c(vals, val)
  }

  names(vals) <- nms
  return(vals)
}

# take command-line parameters
# if of form "name=value" add lits elemet with that name
# if of form "value" just add a list element at that position
parse.args <- function(args=commandArgs(trailingOnly = TRUE))
{
	nms <- list()
	vals <- list()
	if(length(args)==0)return(NULL)
	for(i in 1:length(args))
	{
		keyval <- strsplit(args[i], "=")[[1]]

		if(length(keyval)==1)
		{
			nm <- as.character(i)
			val <- keyval
		}
		else
		{
			nm <- keyval[1]
			val <- keyval[2]
		}

		numval <- as.numeric(val)
		if(is.finite(numval))
		{
			val <- numval
		}
		nms <- c(nms, nm)
		vals <- c(vals, val)
	}

	names(vals) <- nms
	return(vals)
}

# copy list of names argumnents to the environment or list object
apply.args <- function(args, target=parent.frame(), existing.only=TRUE)
{
	is.list <- is.list(target)
	if(is.list)
	{
		for(nm in names(target))
		{
			val <- args[[nm]]
			if(!is.null(val))
			{
				# overwrite
				target[[nm]] <- val
			}
		}
		return(target)
	}
	target <- as.environment(target)
	for(nm in names(args))
	{
		if(exists(nm, envir=target) | !existing.only)
		{
			# attempt to copy values from arguments across to the project
			assign(nm, args[[nm]], envir=target)
		}
	}
	if(is.list)
	{
		target <- as.list(target)
	}
	return(target)
}


# attempt to read the table from the given file. if it fails, return empty table rather falling over
load.table.ex <- function(fn, ...)
{
	dat <- data.frame()
	try(dat <- read.table(fn, ...), silent=TRUE)

}


#source: http://stackoverflow.com/questions/8986818/automate-zip-file-reading-in-r
read.zip <- function(zipfile, row.names=NULL, dec=".") {
	# Create a name for the dir where we'll unzip
	zipdir <- tempfile()
	# Create the dir using that name
	dir.create(zipdir)
	# Unzip the file into the dir
	utils::unzip(zipfile, exdir=zipdir)
	# Get the files into the dir
	files <- list.files(zipdir)
	# Throw an error if there's more than one
	# Throw an error if there's more than one
	if(length(files)>1) stop("More than one data file inside zip")
	# Get the full name of the file
	file <- paste(zipdir, files[1], sep="/")
	# Read the file
	read.csv(file, row.names, dec)
}

MatchClass <- function(obj, names) #c("SpatialLines", "SpatialLinesDataFrame"))
{
	return(class(obj)[[1]] %in% names)

	# return(class(obj)[2] %in% names)

}

# return file path without any extension
# copied from implementation in the tools package
remove_extn <- function(x)
{
	sub("([^.]+)\\.[[:alnum:]]+$", "\\1", x)
}


#' Get one or more GUIDs
#' @description A GUID (globally unique ID) can be used to identify objects such as files uniquely on the system. This method is based on the system time + plus a random number sampled from a uniform distribution
#' @param sep character Separator character
#' @param n numeric Number of guids to return
#' @param max numeric Maximum for randomised element (default 1e6)
#' @return A new GUID based on the system time + plus a random number in 0 to max
new_guid <- function(n=1, sep="-", max=1e5)
{
	# sample without replacement so each is is unique
	rand <- sample.int(max, n)
	# each guid will have the same prefix but unique suffix, could be useful if wanting to identify batches of objects created in one operation
	t <- Sys.time()
	rand <- gsub("\\s", "0", format(rand, width=ceiling(log10(max))+1))
	#guid <- gsub(":|-|\\a|\\s", "-", t)
	# guid is combination of tiem (simulation or actual) and a random number sampled without replacement
	return(paste0(round(as.numeric(t)),"-", rand))
}

# quick way of getting a transparent version of an existing colour
make.transparent = function(..., alpha=0.5) {

	if(alpha<0 | alpha>1) stop("alpha must be between 0 and 1")

	alpha = floor(255*alpha)
	newColor = grDevices::col2rgb(col=unlist(list(...)), alpha=FALSE)

	.makeTransparent = function(col, alpha) {
		grDevices::rgb(red=col[1], green=col[2], blue=col[3], alpha=alpha, maxColorValue=255)
	}

	newColor = apply(newColor, 2, .makeTransparent, alpha=alpha)

	return(newColor)

}

bin <- function(x, breaks=10, ...)
{
	binned <- cut(x, breaks=breaks, labels=FALSE, ...)
	return(binned)
}


# Report SIZE OF object in Mb
mb <- function(x)
{
	return(format(utils::object.size(x), unit="Mb", digits=2))
}


# check that the object is of the stated type
# if stop = TRUE then stop with error message
CheckClass <- function(x, req, stop=FALSE)
{
	if(!is.null(x))
	{
		# case-insensitive check
		res <- toupper(class(x)[[1]]) == toupper(req)
		if(!res & stop)
		{
			stop(paste("Object is of type ", class(x)[[1]], ", but expected type is ", req))
		}
		return(res)
	}
	return(TRUE)
}

# is the input assigned and of the expected type?
CheckClassAndNotNull <- function(x, expected, name)
{
	if(is.null(x)){
		stop(paste("Unexpected NULL input for parameter ", name))
	}
	else if(class(x)[[1]] != expected)
	{
		stop(paste("Unexpected class for input for parameter, ", name,  ": expected ", expected, ", found ", class(x)[[1]]))
	}
	return(TRUE)
}

# check that tabular object has given column names
check.cols <- function(obj, names, stop=TRUE)
{
	msg <- NULL
	for(nm in names)
	{
		if(!(nm %in% colnames(obj)))
		{
			msg <- paste(msg, "Missing column name ", nm, "\n")
		}
	}
	if(length(msg)>0 & stop){stop(msg)}
	return(msg)
}

# synonym for file.path
fp <- function(a,b)
{
	return(file.path(a,b))
}



# default function for expansion of series values
fun.rep <- function(x,fact)
{
	return(rep(x, fact))
}


fun.mean <- function(x,fact)
{
	return(rep(x, fact)/fact)
}

require(xts)


report.multi.series <- function(obs)
{
	res <- lapply(1:ncol(obs), function(icol){report.series(obs[,icol])})
	names(res)<- colnames(obs)
	return(res)

}

# report on bounds and lengths of valid data
report.series <- function(obs, col=1)
{
	if(!is.zoo(obs)){stop("Time series required")}
	if(ncol(obs)>1)
	{
		return(report.multi.series(obs))
	}
	obs <- obs[,col]
	data.periods <- rle(as.vector(!is.na(obs)))

	starts <- index(obs)[c(1, cumsum(data.periods$lengths)+1)]
	ends <- index(obs)[cumsum(data.periods$lengths)]
	df <- NULL
	# run through each period
	#  df <- data.frame()
	for(i.len in 1:length(data.periods$lengths))
	{
		df <- rbind(df, c("start"=format(starts[i.len]), "end"=format(ends[i.len]),
						  valid=data.periods$values[i.len]))

	}

	return(data.frame(df))
}

# list objects in current environment of specified class (class instance)
ls.inst <- function(env=environment(), class.names="object")
{
	res <- list()
	nms <- ls(pos=env)
	mget(nms, envir=env, mode="complex")

}


# dt in hours
# obj a zoo or xts series
# aggregate_xts <- function(from,
#                           to)
# {
#   dt.from <- 1/frequency(from)/3600
#   dt.to <- 1/frequency(to)/3600
#   n.aggr <- dt.to/dt.from
#   if(n.aggr < 1){warning("Observations at lower time resolution than simulation")}
#   # aggregation (e.g quarter hour to hourly)
#   index.agg <- rep(index(from), each=n.aggr)
#   xts.agg <- aggregate(run.sim$qobs, by = index.agg, FUN=mean)
#   return(obj[ind])
# }


# Make sure parameters are supplied (easy to misspell)
CheckParams <- function(obj, names, warn="message", stop=FALSE)
{
	bad <- which(!names %in% obj)
	if(length(bad)>0)
	{
		msg <- paste("Parameters not found ", paste(names[bad], "\n"))
		if(warn=="warn"){
			warning(msg)
		}
		else if("warn"=="message"){
			message(msg)
		}
		else if("warn"=="stop"){
			stop(msg)
		}
		return(FALSE)
	}
	return(TRUE)
}

# check that the named object exists and is initialised
CheckNotNull <- function(obj, stop=FALSE)
{
	if(!exists(obj)|is.null(obj))
	{
		if(stop)
		{
			stop(paste("value for ", obj, " required"))
		}
		return(FALSE)
	}
	return(TRUE)
}

# A one-liner splitting d into chunks of size 20:
chunk <- function(d, n)
{
	split(d, ceiling(seq_along(d)/n))
}


# given a vector P of probabilities adding to 1, return a random integer in the
# interval [1, n(P)], where the probability of selection is weighted according to
# the corresponding weight p
WeightedRandomSelection <- function(p,n=1)
{
	# divide unit interval up in proportion to weights
	q<-cumsum(p)

	if(is.na(sum(p)))
	{
		browser()
	}

	# random slection from uniform dist in [0,1]
	r<- stats::runif(n)

	# in which intervals are the samples located (start at 0, add 1)
	return(findInterval(r,q)+1)

}

# get the drive letter for a fully-qualified or relative (to wd) path.
# includes colon
DriveName <- function(pth)
{
	pth <- path.expand(pth)
	return(strsplit(pth, split="/")[[1]][1])
}

# is argument Null or less than the specified min
NullOrLt <- function(x, min=0)
{
	if(is.null(x))
	{
		return(TRUE)
	}
	if(is.numeric(x))
	{
		return(x<min)
	}
	warning("Non-numeric argument supplied")
	return(TRUE)
}

# if argumane is NULL or lt given value set it to value
FixNullOrLt <- function(x, min=0)
{
	if(NullOrLt(x, min))
	{
		return(min)
	}
	return(x)
}


# last element of vector
Last <- function(x)
{
	if(length(x)>0)
	{
		return(x[length(x)])
	}
	warning("Attempted to find last element of zero-length vector")
	return(NULL)
}

# unique function that removes NA values from result
UniqueExcludingNAs <- function(x,fromLast=FALSE)
{
	x <- unique(x, fromLast=fromLast)
	return(x[!is.na(x)])
}

# statistical mode function
statmod <- function(x)
{
	z <- table(as.vector(x))
	if(length(x[!is.na(x)])==0)
	{
		# nothing non-NA so return this
		return(NA)
	}
	else
	{
		return(names(z)[z == max(z)])
	}
}

# return index of element at row, col pos in matrix in terms of colum major array
# index values
MatrixToArrayIndex <- function(mat,row,col)
{
	if(!is.matrix(mat))
	{ stop("Non-matrix supplied to MatrixToArrayIndex function")}

	return(row + (col-1)*nrow(mat))
}

# Prompt for a user response followed by a CR, taking the default if enter is hit
GetUserResponse <- function(prompt,
							def="y" #  result if user hits enter - default is "yes"
)
{
	resp <- readline(prompt)
	if(resp == ""){resp<-def}
	return(resp=="y")
}

# all values in current environment coipied to target list or environwmnt
CopyVals <- function(from, to,
					 all=FALSE)  #
{
	is.list <- FALSE
	if(is.list(to))
	{
		is.list <- TRUE
		# convert to environment for copying, convert back on return
		to <- list2env(to)
	}

	nms.to <- ls(to)
	for(nm in ls(from, all.names=TRUE))
	{
		if(nm %in% nms.to)
		{
			# assign name-value in this envir to that specified
			assign(nm, get(nm, from), pos=to)
		}
	}
	if(is.list)
	{
		to <- as.list.environment(to)
	}
	return(to)

}

# http://stackoverflow.com/questions/1358003/tricks-to-manage-the-available-memory-in-an-r-session
# What tricks do people use to manage the available memory of an interactive R
# session? I use the functions below [based on postings by Petr Pikal and David
# Hinds to the r-help list in 2004] to list (and/or sort) the largest objects
# and to occassionally rm() some of them. But by far the most effective solution
# was ... to run under 64-bit Linux with ample memory.
.ls.objects <- function (pos = 1, pattern, order.by,
						 decreasing=FALSE, head=FALSE, n=5) {
	napply <- function(names, fn) sapply(names, function(x)
		fn(get(x, pos = pos)))
	names <- ls(pos = pos, pattern = pattern)
	obj.class <- napply(names, function(x) as.character(class(x))[1])
	obj.mode <- napply(names, mode)
	obj.type <- ifelse(is.na(obj.class), obj.mode, obj.class)
	obj.size <- round(napply(names, utils::object.size)/2^20, 1)  # size in Mb
	obj.dim <- t(napply(names, function(x)
		as.numeric(dim(x))[1:2]))
	vec <- is.na(obj.dim)[, 1] & (obj.type != "function")
	obj.dim[vec, 1] <- napply(names, length)[vec]
	out <- data.frame(obj.type, obj.size, obj.dim)
	names(out) <- c("Type", "Size", "Rows", "Columns")
	if (!missing(order.by))
		out <- out[order(out[[order.by]], decreasing=decreasing), ]
	if (head)
		out <- head(out, n)
	out
}
# shorthand
lsos <- function(..., n=10, query=FALSE)
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


clean.up.dirs <- function(dir, pattern="*.*")
{
	fns <- dir(dir, pattern, recursive=TRUE)
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
		if(length(dir(dn, include.dirs=TRUE))==0)
		{
			cat("Removing ", dn, "..\n")
			unlink(dn, recursive=TRUE, force=TRUE)
		}
	}

}


# locate values of var in any calls to given fn on current execution stack
# ignore.caller ignore any values returned from the of calling environment
# var name of object whose values are sought
search_stack <- function(fun, var,
						 ignore.caller=TRUE)
{
	# match function name on line start
	patt <- paste0("^", fun)
	calls <- sys.calls()
	if(ignore.caller){calls <- calls[-(length(calls)-1)]}  # this will always be the calling environment
	icall <- grep(patt, calls)
	# remove the first call
	res <- lapply_ex(sys.frames()[icall],
					 function(x)
					 {
					 	get(x=var, envir=x)
					 }
	)
}


has.attributes <- function(obj, attr_names)
{
	if(is.null(obj))
	{
		warning("has.attributes applied to NULL")
		return(FALSE)
	}
	res <- all(attr_names %in% names(attributes(obj)))
	return(res)
}

# load every source file in given directory
load.source.dir <- function(dn, recursive=FALSE, patt="*.r")
{
	fns <- dir(dn, patt, full.names=TRUE, recursive=recursive, ignore.case=TRUE)
	sapply(fns, source)
}



# 	# examine where we are being called from and if an attempt has already been made to
# 	# load this source by load.source during current execution stack
# 	fns <- search_stack("load.source", "fn")
# 	if(fn %in% fns)
# 	{
# 		message(paste0("Note: detected a nested call to load.source with param ", fn, "; ignored"))
# 		return()
# 	}
# 	subpath <- ""
# 	if(basename(fn)!=fn)
# 	{
# 		# extract a path is one supplied
# 		subpath <- dirname(fn)
# 		# determine whether this directory exists
# 	}
# 	fn <- basename(fn)
# 	pattern <- paste("^", fn, "$", sep="")
#
# 	# locate t
#
# 	srchpths <- c("~/source")# unique(#c(getwd(),
# 									#		 pths,	 Sys.getenv("SRC_DIR"), "~/source"))
#
# 	# recursively search from current INCLUDE dir to locate and load source file with given name
# 	src <- sapply(srchpths,
# 								function(pth)
# 								{
#                   if(file.exists(pth))
#                   {
#   									setwd(pth)
#   									if(subpath == "" || file.exists(subpath))
#   									{
#   										#			browser()
#   										# either a subpath is specified that can be loacted under one of the search dirs
#   										# or none specifed, in which case just search for file recursively in dirs
#   										src <- dir(pattern=pattern, ignore.case=TRUE,  recursive=TRUE, full.names=FALSE)
#   										src <- file.path(pth, src)
#   									}
#                   }
# 								}
# 	)
# 	src <- delete.NULLs(src)
# 	#	if(length(src)>0){break()}  # found a file with this name
# 	src <- unlist(src)
# 	if(length(src)==0)
# 	{
# 		warning(paste("Cannot locate ", fn, " on paths ", paste(srchpths, "\n")))
# 		return(FALSE)
# 	}
# 	# remove duplicates
# 	src<-unique(src)
# 	if(length(src)>1 & warn)
# 	{
# 		#   message(paste0("Multiple files located: \n", paste(src, "\n")))
# 		warning(paste0(src, "\n"), "Loaded ", src[[1]])
# 	}
# 	fn <- src[[1]]
# 	message("Loading source file: ",fn)
#
# 	source(fn, chdir=TRUE, keep.source=TRUE, local=local)
# 	if(chdir)
# 	{
#     # this allows sourced files to source files in same location
# 		try(setwd(dirname(fn)))
# 	}
#
# 	return(TRUE)
#}

# install if required then load packages in list supplied
LoadPackages <- function(pnames, attach=FALSE, ...)
{
	# what's installed already
	installpackages <- setdiff(pnames, utils::installed.packages()[,1])
	# install anything that's not on the list

#	iwarn <- options("warn")
	options("warn"=-1)
	if(length(installpackages)>0){
	  utils::install.packages(installpackages, quiet=TRUE, ...)
	}
	if(attach)
	{
		# "require" each of the specified packages
		#lapply(as.list(pnames), function(p){require(p, character.only=TRUE)})
	}
#	options("warn"= iwarn)
}

# http://stackoverflow.com/questions/1815606/rscript-determine-path-of-the-executing-script
thisFile <- function() {
	cmdArgs <- commandArgs(trailingOnly = FALSE)
	needle <- "--file="
	match <- grep(needle, cmdArgs)
	if (length(match) > 0) {
		# Rscript
		return(sub(needle, "", cmdArgs[match]))
	} else {
		# 'source'd via R console
		return(normalizePath(sys.frames()[[1]]$ofile))
	}
}

this.dir <- function()
{
	return(dirname(thisFile()))
}

# return a file name created from an object with a names property
get.obj.fn <- function(dn, obj, ext="rdata")
{
	if(length(names(obj))==0)
	{
		stop("No names supplied in object")
	}
	fn <- file.path(dn,
					paste0(paste0(names(obj), "=", obj, collapse="_"), ".", ext))
	return(fn)
}

# remove empty directories
remove.empty <- function(dn)
{
	sapply(list.dirs(dn),
		   function(d)
		   {
		   	nf <- length(dir(d, recursive=TRUE))
		   	if(nf==0)
		   	{
		   		cat("Deleting directory ", d, "\n")
		   		file.remove(d)
		   	}
		   },
		   USE.NAMES=FALSE
	)
}

apply.vals <- function(target, params)
{
	if(is.null(names(target)))
	{
		warning("Can't locate names in target to which to apply values")
		return(target)
	}
	# select parameters in target
	params <- params[which(names(params) %in% names(target))]
	if(length(params)==0)
	{
		# No corresponding values in the target
		warning("Can't locate any names in target to which to apply values")
		return(target)
	}

	if(is.list(target))
	{
		# just apply the vals to the matching list elements
		target[names(params)] <- params
		return(target)
	}
	napp <- max(nrow(target), 1, na.rm = TRUE)
	vals <- matrix(rep(unlist(params), napp), nrow=napp, byrow=TRUE)
	if(napp>1)
	{
		target[,names(params)]<- vals
	}
	else
	{
		# same as for list?
		target[names(params)]<- vals
	}
	return(target)
}

###############################
# date time functions

# converting to no. seconds since 1st Jan 1970
as.numeric.date.time <- function(t)
{
	as.numeric(as.POSIXct(t))
}

# converting from no. seconds since 1st Jan 1970
from.numeric.date.time <- function(ns)
{
	as.POSIXct(ns, origin="1970-01-01")
}

dir.empty <- function(dn)
{
	fns <- dir(dn, full.names = TRUE)
	file.remove(fns)
	nrem <- length(fns)
	message(nrem, " file(s) removed")

}


