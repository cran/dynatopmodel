#######################################################################
# Useful routine for calibration with multiple runs
#######################################################################
cleanup.calib.dir <- function(dn, backup=TRUE)
{
	
	fns <- dir(dn, "*.dat", full.names=TRUE, include.dirs = FALSE)
	
	if(readline(paste0(lengths(fns), " files will be removed. Continue (y)?"))!="y")
	{
		return(NULL)
	}
	
	if(backup)
	{
		backup.dir <- file.path(dn, format(Sys.time(), "%Y-%m-%d_saved"))
		if(!file.exists(backup.dir))
		{
			dir.create(backup.dir)
		}
		res <- file.copy(from = fns, to=backup.dir)
	}
	try(file.remove(fns), silent = TRUE)
	message(paste0(length(which(res)), " file(s) moved to directory ", backup.dir))
}



get.results.dir <- function(dn, run.par, fmt="%Y-%m-%d")
{
	p1 <- format(run.par$start, fmt)
	p2 <- format(run.par$end, fmt)
	sd <- paste0(p1,"_", p2)
	dn <- file.path(dn, sd)
	if(!file.exists(dn))
	{
		dir.create(dn, recursive=TRUE)
	}
	return(dn)
	
}

get.case.basename <- function(params, dn=NULL, id=NULL, ext=NULL, sep="_")
{
	# add and identifier
	params <- c("id"=id, params)
	nm <- paste0(paste0(names(params), "=", params, sep="", collapse=sep))
	if(!is.null(ext))
	{
	  # making sure extensions with both preceeding periods and not are treated in the same way
	 ext <- gsub("/.", "", ext)
	 nm <- paste0(nm, ext)
	}
	
	if(!is.null(dn))
	{
	  nm <- file.path(dn, nm)
	}
	return(nm)
}


# determine no. calibration sets left
remaining_sets <- function(calib.dat, calib.sets)
{
	
}

load_calib_results <- function(calib.results.dir, 
															 crit=list("NSE"=0.8),
															 suff=".dat$",
															 load=FALSE)
{
	fns <- dir(calib.results.dir, suff)
	
	dat <- NULL
	cat("Loading ", length(fns), " runs...")
	nms <- NULL
	
	for(fn in fns)
	{
	#	load(file.path(calib.results.dir, fn))
		#	if("props" %in% slotNames(res))
		cat("Checking run ", fn, "...")
		
		# parse each file name to determine the parameters for the run
		#fn <- sub("brom_full_", "", fn)
		args <- sub(suff, "", fn)
		args <- sub("sd_max", "sd.max", args)
		args <- sub("ln_t0", "ln.t0", args)
		args <- sub("srz_max", "srz.max", args)
		args <- strsplit(args, "_")[[1]]

		vals <- parse.args(args=args)

		#		nms <- c(nms, names(res@props))
		vals <- c(fn, vals)
		
		include <- TRUE
		if(length(crit)>0)
		{
			# check that the case meets the specified criterion and reject if not
			for(nms.crit in intersect(names(vals), names(crit)))
			{
				if(as.numeric(vals[nms.crit]) <  crit[nms.crit])
				{
					include <- FALSE
				}
			}
		}
		if(include)
		{
			if(load)
			{
				
			}
			else
			{
				# read the contents
				vals <- read.table(file.path(calib.results.dir, fn),
												 sep="\t", header=TRUE)
				if(is.null(nms))
				{
					# the first included result sets the expected values
					nms <- colnames(vals)
				}
				dat <- rbind(dat, vals)
			}
			names(dat) <- nms	
			cat("... included\n")
		}
		else
		{
			cat("... not included\n")
		}

	}

	if(length(dat)>0)
	{
	dat <- data.frame(dat, row.names=1:nrow(dat))
	}
	else
	{
		message("No results loaded")
	}
	
	names(dat) <- nms
	
	return(dat)
}

load_contents <- function(fn)
{
	
}


# apply a list of named values to the existing data table or list
# if names missing in target then add to target
# if a list of parameters to ignore provided do not overwrite these in the target
apply_params <- function(groups, params, ihru=1:nrow(groups), preserve=NULL,
                         existing.only=FALSE)
{
  
  nms <- names(params)
  
  if(existing.only)
  {
    # only apply the parameters in the target 
    nms <- intersect(names(params), names(groups))
  }
  #  params <- params[nms]
  
  # miss out any given named parameters from the target
  if(length(preserve)>0)
  {
    igood <- which(!(nms %in% preserve))
    nms <- nms[igood]
  }
  
  # igood <- which(!is.na(names(params)))
  params <- params[nms]
  
  
  if(length(params)==0)
  {
   # warning("No valid parameters found to apply")
    return(groups)
  }
  
  for(nm in names(params))
  {
    if(is.list(groups))
    {
      groups[[nm]] <- params[[nm]]  
    }
    else if(is.data.frame(groups))
    {
      groups[ihru,nm] <- params[[nm]]
    }
    #  catch_discretisation[[nm]] <- params[[nm]]
  }
  return(groups) 
  
}

