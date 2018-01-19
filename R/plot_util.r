# useful axis plotting fuction with custome grid lies
add.q.axis <- function(side=2, qsim, ylim=range(qsim), int=0.1, lab="", 
                       line=2.5, cex=par("cex.axis"),
                       col="gray", lty=2,
                       las=3,...)
{
  at <- seq(floor(min(qsim, 0, na.rm=TRUE)), ceiling(ylim[2]), by=as.numeric(int))#  pretty(par("usr")[3:4])
  #  at <- at[-length(at)]
  if(side %in% c(2,4))
  {
    abline(h=at, col=col, lty=lty)
    abline(h=0, col=col)
  }
  else
  {
    abline(v=at, col=col, lty=lty)
    abline(v=0, col=col)
    
  }
  axis(side=side, at=at, cex.axis=cex, cex.lab=cex)
  # neater
  mtext(text=lab, las=las, side=side, line=line,
        font=par("font.lab"),
        cex=cex) #par("cex.lab"))
  
}


# Legend at BR under plot
add_legend <- function(nrow=2,
											 legend = expression("Simulated",
											 										"Observed",  "Precipitation",
											 										E[a]),
											 x=xlim[2],
											 lwd=2,
											 lty=1,
											 cex=par("cex"),
											 col = c("blue", "green", "black", "brown"),
											 title=NULL, 
											 yoff=-0.05,...
)
{
	if(length(legend)>0)
	{
		#  determine plot limits
		xlim <- par("usr")[1:2]
		# xjust and yjust controls how legend justified wrt x and ycoord: 2=right / top justified (doc is wrong)
		legend(x=x, 
					 y=yoff, 
					 lwd=lwd,
					 lty=lty,
					 legend=legend, #ncol=length(titles),
					 title=title,
					 cex=cex,
					 xpd=TRUE,  # needed in order to plot outside figure margin
					 yjust=2, xjust=1, horiz=TRUE,
					 # ncol=max(2, round(length(titles)/nrow+0.5)),
					 col=col,# box="n",
					 bg="white", ...)
		#bg="#FFFFFFCC")
		
	}
}


# utilities for plotting graphs, partcular those display time series data
add_time_axis <- function(side=1,
													las=par("las"),
													labels=TRUE,
													time.int="week",
													col="slategray", lty=2,
													cex=par("cex.lab"),
													fmt="%d-%b-%y",...)
{
	#grid(col="slategray", nx=NA)
	#  horz axis above with perpendicular labels - more ticks than default- compute tick locations
	par("xaxp"=c(par("usr")[1:2], 1))
	# time axis at top, add formatted labels and lines
	
	# round_date is a lubridate method
	tm.range <- lubridate::round_date(as.POSIXct(par("xaxp")[1:2], origin="1970-01-01"), "day")
	tms <- seq(tm.range[1], tm.range[2], by=3600)		# hourly intervals
	
	if(is.numeric(time.int))
	{
		i.at <- which((lubridate::hour(tms)+24*lubridate::day(tms)) %% time.int==0)
	}
	else
	{
		# more detail. get pretty breaks based on day numbers (date-times in seconds from t.origin)
		# use more lubridate methods
		i.at <- switch(time.int,
									 "year"=which(lubridate::yday(tms)==1 & lubridate::hour(tms)==0),
									 "quarter"=which((lubridate::month(tms)-1)%%3==0 & lubridate::mday(tms)==1 & lubridate::hour(tms)==0),
									 "month"= which(lubridate::mday(tms)==1 & lubridate::hour(tms)==0),
									 "week"= which(lubridate::wday(tms)==1 & lubridate::hour(tms)==0),
									 "day"=which(lubridate::hour(tms)==0),
									 "hour"=which(lubridate::second(tms)==0)
		)
	}
	
	at <- tms[i.at]
	
	if(labels)
	{
		
		labs <- at  # as.POSIXct(at, origin="1970-01-01")
		labs <- format(labs, format=fmt)
		for(iside in side)
		{
			axis(side=iside, at = at, las=las, labels=labs, cex.lab=cex, ...)
		}
	}
	abline(v=at, col=col, lty=lty)
	
}

# plot the supplied time series together 
plot_all <- function(...)
{
	parms <- list(...)
	ts <- lapply(parms,
								function(x)
								{
									if(is.zoo(x))
									{
										return(x)
									}
									return(NULL)
								}
	)	
							
			
	par <- lapply(parms,
									 function(x)
									 {
									 	if(!is.zoo(x))
									 	{
									 		return(x)
									 	}
									 	return(NULL)
									 }
	)	
	ts <- do.call(cbind, ts) 
	
	do.call(plot.zoo, c(list(ts), plot.type="single", par))
}

disp.sim.time <- function(tm, label=TRUE, fmt="%d-%b-%y", col="red", lty=3, lwd=2, ...)
{
	if(label)
	{
		time.str <- format(tm, fmt)
		mtext(side=1, at=tm, text=time.str, las=3,
					cex=0.8, line=0.25)
	}
	abline(v=tm, col=col,lty=lty, lwd=lwd)
}
