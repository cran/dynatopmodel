# Outputting of Dynamic TOPMODEL results
#############################################################
update.output <- function(output, groups, stores, it, ichan,
                          items=NULL)
{
  # weighted average of storage deficits
 # output[time, "sd"]<-
  # record river input flow
 # output[time, "qriv.in"] <- sum(Qchan)

  # water balance: rain in, evap and specific discharge out
#  output[it, "wb"]  <- output[it, "wb"] + as.numeric(current.input(groups, rain[it,], output[it, "ae"], Qr[it,]/catchArea))
#  output[time, "ae"] <- weighted.mean(ae, groups$area)
#  storage.deficits[time,]<- stores[-ichan, "sd"]
#  base.flows[time,]<- flows[-ichan, "qbf"]
  for(nm in names(items))
  {
    if(!nm %in% names(output))
    {
      output <- cbind(output, rep(0, nrow(output)))
      names(output)<-c(names(output)[1:(length(names(output))-1)], nm)
    }
    output[it, nm] <- items[[nm]]

  }

    return(output)
}


#
RunSummary <- function(groups, stores, storage.in, ichan, qsim, qobs, start.time,
                       rain, ae=0, text.out)
                     #  sat.ex =0,
                    # qbf.ex = 0, rain=0,
                    #   ae=0)
{
  if(!is.null(text.out)){return()}
  run.time <- difftime(Sys.time(), start.time, units="auto")
  # depending on length of run could be mins, secs (or hours!)
  units <- units(run.time)
  # difference between net input and net storage gain
  storage.gain <- current.storage(groups, stores, ichan)-storage.in
  wb <- water.balance(groups, stores, storage.in, ichan, qsim,
                       rain, ae)

  #sum(rain-ae)-sum(qsim)-storage.gain

  LogEvent(paste("Run completed in ", round(run.time), units))
  # water balance etc
  cat("Total river discharge      = ", round(sum(qsim),2), "m\n")
#  cat("   saturated excess        = ", round(sum(sat.ex),3), "m)\n")
#  cat("   base flow excess        = ", round(sum(qbf.ex),3), "m)\n")
  cat("Actual  evapotranspiration = ", round(sum(ae),2), "m\n")
  cat("Total rain input           = ", round(sum(rain),2), "m\n")
  cat("Net storage gain           = ", round(sum(storage.gain),2), "m\n")

  cat("Water balance              = ", round(wb,2), "m\n")

}





# ###########################################
# Routines for displaying simulation results
#############################################


disp.discharge.selection <- function(groups,
                                      qsim,  # in m/hr
                                      qobs,  #  "
                                      rain, evap,
                                      sel=NULL,
                                      max.q=0,   # upper bound for discharge display, in mm/hr
                                      ichan=1,
                                      disp.par=disp.par,
                                      title=disp.par$title.main,...
                                      )

{
  if(is.null(sel))
  {
    sel<-range(index(qsim))
  }

  buf <- as.numeric(difftime(sel[2], sel[1], units="hours")) #[2]-sel[1]

  ae <- xts(1000*subset_zoo(evap, sel[1], sel[2]))
  if(!is.null(qobs))
  {
    qobs <- xts(1000*subset_zoo(qobs, sel[1], sel[2]))}
  qsim <- xts(1000*subset_zoo(qsim, sel[1], sel[2]))

  mar <- c(4, 4, 5, 5)
  if(!disp.par$legend.show)
  {
    # less space at base
  #  mar[1]<-2
  }
  if(title=="")
  {
    # don'TRUE need so much space at top
    mar[3]<-3
  }
    par("mar"=mar)

  if(!is.null(qobs))
  {
    #names(qobs)<-"Observed discharges"

  }
  #ae <- 1000*evap   #[,"ae"]

  disp_output(title=title, disp.par=disp.par, runid=NULL,
                    qsim, ae, 1000*rain, tm=sel[1],
                    groups, qobs, #eff=eff,
                    ymax=max.q,

                                 # show to end of simulation period
                    timeBuffer= buf,
                    ichan=ichan,...)

}

# the beginning of the epoch is 1970, so coercing a POSIXct to numeric gives
# no. secs since 01-01-1970:00:00
asPOSIXct <- function(x, origin = "1970-01-01", tz = "GMT")
{
  return(as.POSIXct(x, origin = origin, tz = tz))

}


# returns the size of an time based object in seconds or ghours
length.period <- function(qr, units="hr")
{

	res <- diff(as.numeric(range(index(qr))))

	if(units=="hr")
	{
		res <- res/3600
	}
	return(res)
}


get_daily_maxima <- function(dat, freq="day")
{
	# split time series by day
	day_vals <- split(dat, f = "day")

	day_max <- sapply(day_vals, max)

	# position of maximum within day
	imax_day <- sapply(day_vals,
									function(x)which.max(x))

	# cumulative total number of records at the day
	ilen <- c(0, cumsum(sapply(day_vals, nrow)))

	# index of the maximum in each day within the donor series
	ipos <- ilen[1:length(imax_day)] + imax_day

	# returning the daily maxima (xts)

	return(dat[ipos,])

}


# Output results of a Dynamic TOPMODEL run. Wrapper to disp.output
# qsim	xts Supply if a time series other than that held by the run is displayed
# show.daily.maxima Boolean If TRUE then the daily maxima of the series are shown as crosses
plot.run <- function(run,
                     qsim=NULL,
                     start = run$sim.start,
                     end = run$sim.end,
                     fact=1000,
                     par=get.disp.par(),
										 show.maxima=FALSE,
                     ...)
{
  sel <- paste0(start, "::", end)
  rain <- run$rain[sel]*fact
  evap <- run$ae[sel]*fact

  par <- merge.lists(par, list(...))
  # observations, if supplied
  qobs <- run$qobs

  if(is.null(qsim)){
    # use the run output - can over
    qsim <- run$qsim[sel]*fact
  }

  if(!is.null(evap)){
  	# showing evapotranspiration
    ae <- evap[sel]
  }
  else{
    ae <- NULL
  }

  qsim <- qsim[sel]
  par$max.q <- max(c(par$max.q, qsim[]), na.rm=TRUE)

  if(!is.null(qobs))
  {
  	# obsevation dat supplied - scale (if necessary) and select
    qobs <- qobs[sel]*fact
    try(print(NSE(qsim, qobs), silent=TRUE))
    # update the limits
    par$max.q <- max(c(par$max.q, qobs), na.rm=TRUE)

    # stats
    cat("Observed time at peak = ", format(time_at_peak(qobs)), "\n")
    cat("Observed peak discharge = ", round(max(qobs),2), " mm/hr\n")
  }
  cat("Time at peak = ", format(time_at_peak(qsim)), "\n")
  cat("Peak discharge = ", round(max(qsim),2), " mm/hr\n")

  #
  disp_output(qsim=qsim,
              #		start=sim.start,
              #		end=sim.end,
              evap=evap,
              rain=rain,
              tm=NULL,
              start=start,
              end=end,
              qobs=qobs,
              show.maxima=show.maxima,
              par=par, ...)

}

#' Display output of a Dynamic TOPMODEL  run
#'
#' @description Simple output of the results of a simulation.
#'
#' @details This will render the hydrograph, any observations, actual evapotranspiration, if supplied, and the rainfall hyetograph.
#' @export disp_output
#' @param qsim Time series of simulated discharges.
#' @param rain Time series of rainfall, at same interval as simulated values.
#' @param evap Time series of evapotranspiration (optional), at same interval as simulated values.
#' @param qobs Time series of evapotranspiration (optional), at same interval as simulated values.
#' @param start Start time for plot in a format interpretable as POSIXct date time. Defaults start of simulated discharges.
#' @param end End time for plot in a format interpretable as POSIXct date time. Defaults to end of simulated discharges.
#' @param tm Display a vertical line at this time in the simulation. If NULL no line will be drawn.
#' @param show.maxima Boolean Whether to show the daily maxima as points
#' @param freq character Period for which maxima are identified, day by default
#' @param pch.qsim  character Symbol for plotting maxima of simulations, if stipulated
#' @param pch.obs  character Symbol for plotting maxima of observations, if these are supplied
#' @param par Parameters controlling display output. A default set may be obtained through a call to disp.par.
#' @param ... Any further named parameters will be treated as graphics parameters and applied throughout the plot.
#' @author Peter Metcalfe
#' @seealso disp.par
#' @examples
#' \dontrun{
#' # Show the output of the storm simulation, overriding label colours and vertical axis limits.
#' require(dynatopmodel)
#'
#' data(brompton)
#'
#' x11()
#' with(brompton$storm.run, disp_output(evap=ae*1000,qobs=qobs*1000,
#'                                      qsim=qsim*1000, rain=rain*1000,
#'                                      max.q=2, cex.main=1, col.axis="slategrey", las.time=1))
#'}
disp_output <- function(qsim,
                        rain, evap=NULL,
                        qobs = NULL, tm=NULL,
                        start=min(index(qsim)),
                        end=max(index(qsim)),
												par=get.disp.par(lwd.rain=3, qint=0.1),
												show.maxima=FALSE, freq="day", pch.qsim="+", pch.obs="*",
												...)
{
  # display parameters merged with anything supplied in extra ellipsis
  par <- merge.lists(par, list(...))
  # save and restore display parameters
  old.par <- par(no.readonly = TRUE)
  on.exit(par(old.par))

  # any relevant graphical parameters supplied in disp.par will applied
  par(delete.NULLs(par[names(old.par)]))

  # add more space to right for axis titles, trim left margin a bit and shrink bottom margin
 # par("mgp"=c(2,1,0))
  par("xpd"=FALSE)

  # if the time is specified then extend the time bounds around it
  if(!is.null(tm))
  {
    timeBuffer <- par$graphics.window.length # in hours
    # Window onto the desired period in the simulation, or all of it if limits not specified. Assume buffer in hours
    # rainfall and evapotranspiration on same plot (conversion to seconds for POSix time)
    start  <- max(c(tm-timeBuffer*3600/2, start, na.rm=TRUE))
    end <- max(c(start+timeBuffer*3600, end, na.rm=TRUE))

    # if past the end of the rainfall stop there
    if(end>max(index(rain)))
    {
      end <- max(index(rain))
    }
  }

  # subset the data to only this peiod
  sel <- paste0(start, "::", end)
  rain <- rain[sel]
  qsim <- qsim[sel]
  qriv <- qsim
  nobs <- 0
  cols <- par$col.qsim

  lty <- par$lty.qsim
  lwd <- par$lwd.qsim

  leg.txt <- expression(q[sim]) #"Simulated"
  # if just one colour supplied for multiple simulations assume that this is applied to all of them
  nsim <- ncol(qsim)
  if(nsim >1 & length(cols) < nsim)
  {
    cols <- rep(cols[1], nsim)
  }

 	if(length(qobs)>0)
 	{
 	  # if observations supplied add to output and colours plus vector of line widths and types used in disp.qsim
 		qobs <- qobs[sel]
 		qriv <- cbind(qriv, qobs)
 		nobs <- ncol(qobs)
 		cols <- c(cols, par$col.qobs[1:nobs])
 		lty <- c(lty, rep(par$lty.qobs, nobs))
 		lwd <- c(lwd, rep(par$lwd.qobs, nobs))
 		for(i in 1:nobs)
 		{
 		  leg.txt <- c(leg.txt, expression(q[obs]))
 	  }# "Observed"
 	}

  leg.txt <- c(leg.txt, "Precipitation")

  xlim <- range(index(qriv))

  # rain at top, inverted
  layout(matrix(1:2), heights=c(par$prop, (1-par$prop)))
  on.exit(layout(matrix(1)))
  # no margin at base of this frame, sits on top of the discharge pane w/o gap
  with(par, par("mar"=c(0,xmar,ymar,xmar)))

  # inverted plot of rainfall (limits reversed)
  disp.rain(rain,
            xlim = xlim,
            ylim = c(par$max.rain, 0),
            lwd=par$lwd.rain,
  				#	cex=par$cex,
  				#	cex.axis=par$cex,
            main=title)

  # axis is labelled if the position specified is at the top
  with(par, add_time_axis(side=3,
                las=las.time,
                labels=time.axis.side=="top",
                cex=par$cex,
                fmt=time.fmt,
                time.int=par$int.time))

  # border up the sides and top
  axis(side=3, col.ticks = NA, labels = FALSE)
  axis(side=2, col.ticks = NA, labels = FALSE)

  # display the current simulation time as a red line - this shows it at top
  disp.pos <- !is.null(tm) && (tm>start) && (tm<end)

  if(disp.pos)
  {
    # a line at the current position
    disp.sim.time(tm, label=FALSE, lty=1, lwd=1)
  }

  title(main=par$main)

  # calculate y limits from maximum of specified values evap, observed flows plotted on LH axis
  ylim <- c(min(qsim, 0, na.rm=TRUE), max(c(0.01, par$max.q), na.rm=TRUE))


  with(par, par("mar"=c(6,xmar,0,xmar)))

  evap <- evap[sel]
	if(length(evap)>0 && length(which(is.finite(evap)))>0)
	{
	  leg.txt <- c(leg.txt, expression(E[a]))
	  disp.evap(evap, ylim=ylim,
	  					cex=par$cex,
	  					col=par$col.evap)
	  par(new=TRUE)
	}

  # simulated and observed discahrges
	disp.qsim(qriv, xlim=xlim, ylim=ylim,
	          cols=cols,
					  qlab=par$lab.q,
						lty=lty,
						lwd=lwd,

						cex=par$cex,
						qint=as.numeric(par$int.q))

	if(show.maxima)
	{

		# daily maxima for the simulated discharges (or at frequency supplied in FALSE)
		q_max_sim <- get_daily_maxima(qsim, freq=freq)
		# Show only values over QMED
		iper <- which(q_max_sim>quantile(q_max_sim, probs=0.5))
		q_max_sim <- q_max_sim[iper,]
		#max_obs <- get_daily_maxima(qriv[2]=FALSE)
		graphics::points(x=index(q_max_sim), y=q_max_sim, pch=pch.qsim, col=par$col.qsim)

		if(!is.null(qobs))
		{
		  q_max<- get_daily_maxima(qobs, freq=freq)
		  iper <- which(q_max >quantile(q_max, probs=0.5))
		  q_max <- q_max[iper,]

		  #max_obs <- get_daily_maxima(qriv[2]=FALSE)
		  graphics::points(x=index(q_max), y=q_max, pch=pch.obs, col=par$col.qobs[1])
		}
	}

  # line showing current simulation time, and time label if desired
	# set the display parameter par$time.pos.fmt to NULL or empty string to supress
	if(disp.pos)
	{
	  label <- length(par$time.pos.fmt)>0
	    disp.sim.time(tm, fmt=par$time.pos.fmt,
	                  label=label,
	                  lwd=1, lty=1)
	}

	# U-shaped borders
  box(bty="u")

  # time axis lines
  with(par, add_time_axis(side=1,
                               las=las.time,
                               cex=par$cex,
                               # axis is labelled if the position specified is at the bottom
                               labels=time.axis.side=="bottom",
                               fmt=time.fmt,
                               time.int=par$int.time))

  if(par$legend.show)
  {
    legend.col <- c(cols, "black", par$col.evap)

    add.legend(legend.col=legend.col,
               legend=leg.txt,
               cex=par$cex,
               yoff=par$legend.yoff)

  }


}


# plot simulated discharges
disp.qsim <- function(qsim, xlim=range(index(qsim)),
											prop=1,
											ylim=c(0, max(qsim)),
											cols=qsim.cols(qsim),
											qint=0.5,
											qline=2.5,
											qlab="Specific discharge (mm/hr)", ...)
{
  if(prop<1)
  {
      # fit to base of screen
      xflims <- par("fig")[1:2]
      par(fig=c(xflims, 0, prop))
      on.exit(par(fig=c(xflims,0,1)))
  }

  plot.zoo(qsim, main="", plot.type="single",
           xlim=xlim,
           ylim=ylim,
           ylab="", #Specific discharge / evap. (mm/hr)",
           col=cols,
           xaxt="n",
           yaxt="n",
           bty="n", xlab="", ...)

  add.q.axis(side=2, qsim, ylim, int=qint, line=qline) #lab=qlab)
  # horizontal grid lines up to max discharge
 #	ylim <- range(qsim)

  at <- seq(floor(min(qsim, 0, na.rm=TRUE)), ceiling(ylim[2]), by=qint)#  pretty(par("usr")[3:4])
#  at <- at[-length(at)]
  abline(h=at, col="gray", lty=2)
  abline(h=0, col="gray")
  axis(2, at=at)
  # neater
  mtext(text=qlab, las=3, side=2, line=2.5,
        font=par("font.lab"),
  			cex=par("cex.lab"))
 #add_time_axis(side=1, las=2)

}


# display actual evapotranspiration, in mm/hr. Axis is at least ymin high
disp.evap <- function(evap,
                      ylim=par("yaxp")[1:2],
                      ymin=1,    # min axis value in mm/hr
                      prop=1,text=expression(E[a]*"(mm/hr)"),
                      cex=par("cex.axis"),
											col="brown",
											...)
{
#	evap[evap==0]<- NA
  if(prop<1)
  {
	  par(fig=c(0,1, 0, prop))
	  on.exit(par(fig=c(0,1,0,1)))
  }
#	if(length(which()))
    plot.zoo(evap, type="h", ylim=ylim, col=col, #make.transparent("brown"),
             xaxt="n", bty="n",
             yaxt="n", main="", ann=FALSE, lty=1, lwd=1, ylab="",xlab="", ...)
#	axis.range <- range(evap, na.rm=TRUE)
  # ensure axis is always at least ymin high, greater if the evap exceeds this
	labs <- seq(0, ceiling(max(c(ymin, evap), na.rm=TRUE)), by=0.2)
	axis(side=4, labels=labs, at=labs, cex.lab=cex)
	mtext(text=text, las=3,
				side=4, line=2.5, adj=0,
				cex=cex)
}

# inverted plot of rainfall
disp.rain <- function(rain, prop=1, col=rain.cols(rain),
                      xlim=range(as.numeric(index(rain))),
                      ylim=c(max(rain,na.rm=TRUE),0),
                      lwd=3,
                      text="Precipitation (mm/hr)", axes=TRUE, side=4,
											cex.axis=par("cex.axis"),
                      cex.lab=par("cex.lab"), line=2, #*cex,
                      ...)
{
    if(prop<1)
    {
        # place in upper part of screen
        xflims <- par("fig")[1:2]
        par(fig=c(xflims, 1-prop, 1))   #
        on.exit(par(fig=c(xflims,0,1)))
    }

  # omit periods without rainfall
  rain[rain==0] <- NA
  plot.zoo(rain,
           xlim=xlim,
           ylim=ylim, type="h", plot.type="single",
           ylab="",
           xaxt="n",
           yaxt="n",
           lend="square",
           #cex.main=1,
           bty="n",
           lwd=lwd,
           col=col, #rain.cols(rain),
           xlab="")


	if(axes)
	{
		# right hand axis - precipitation and pe
		at <- pretty(range(rain, na.rm=TRUE))
		axis(side=side, at=at, labels=at, cex.lab=cex.lab)

		# grid lines
		abline(h=at, lty=3, col="gray")
		#     add_time_axis(side=3, labels=FALSE)

		mtext(text=text, las=3,
					side=side, line=line,
			#		font=par("font.lab"),
					cex=cex.axis)
	}

}

rain.cols <- function(rain)
{
  cols <- c("#000000","slategray","gray")
  return(cols[1:ncol(rain)])
}


qsim.cols <- function(qsim)
{
  cols <-   rev(c("#00FFFF","#00BFDF","#007FBF","#003F9F","#000080")) #colorRampPalette(c("navy", "cyan"), ncol(qsim))

  return(cols[1:ncol(qsim)])
}

add.legend <- function(nrow=2,
                       legend = expression("Simulated",
                                            "Observed",  "Precipitation",
                                            E[a]),
                       cex=par("cex"),
                      legend.col = c("blue", "green", "black", "brown"),
                       legend.title=NULL,yoff=-0.05,...
                       )
{
  if(length(legend)>0)
  {
    #cols <- get.qobs.cols(qobs)
#     titles <- c(paste("Simulated: ", colnames(qsim)),
#                 paste("Rain:", colnames(rain)),
#                 paste("Observed:", colnames(qobs)),
#                 "Evapotranspiration")




    #  determine plot limits
    xlim <- par("usr")[1:2]
    # xjust and yjust controls how legend justified wrt x and ycoord: 2=right / top justified (doc is wrong)
    legend(x=xlim[2], y=yoff, legend=legend, #ncol=length(titles),
           title=legend.title,cex=cex,
           xpd=TRUE,  # needed in order to plot outside figure margin
           yjust=2, xjust=1, horiz=TRUE,
          # ncol=max(2, round(length(titles)/nrow+0.5)),
           col=legend.col, bg="white", lty=1, lwd=2)
    #bg="#FFFFFFCC")

  }
}



# barplot showing channel and reservoir storages
disp.chan.stores <- function(groups, stores, ichan)
{
	chan.stores <- stores[ichan,]
	chan.groups <- groups[ichan,]
	nchan <- length(ichan)
	# scale to percentage?
	chan.groups$sd_max <-0.01
	dat <- rbind(chan.groups$sd_max-chan.stores$sd, chan.stores$sd)
	cols <- rbind(rep("blue", nchan), rep("white", nchan), names.arg = chan.groups$id)
	barplot(horiz=TRUE, dat, col=cols, xlab="Average depth (m)")

}

disp.stores <- function(groups,stores,ichan=1, nlev=5)
{
  # colours for unsat zone
  uzcols <- colorRampPalette(c("#A0D600FF", "blue"))(nlev)

  # move axis title
  par("mgp"=c(1,1,0))
  # add a bit more space at the bottom for the legend and HSU IDs
  par("mar"=c(4, 2, 1, 3))

  groups <- groups[-ichan,]  #rbind(groups[-ichan,], dumgr)
  stores <- stores[-ichan,]  #rbind(, dumst)

  ngroup <- nrow(groups)

  # stacked plot of:
  # max and actual root zone storage
  # unsat zone storage - shown by shading of unsat zone
  # max and actual storage deficit
  # colours for storage diagram
  # tan, brown, light blue, dark blue
  # the uz zone can contain at max the remaining storage deficit?
 # browser()
#   sds <- stores$sd
#   sds[sds==0]<-1
#   suzProp <- stores$srz/sds
  suzProp <- rep(1, ngroup)
  unsat <- which(stores$sd>0)

  uzcol <- uzcols[suzProp]
#   col <- c("red",     # storage excess
#         #   "#A6611A", # dark brown - wetted root zone
#            "#DFC27D", # tan - dried out root zone
#            "#80CDC1", # green - unsat
           col<-c("gray", # bed rock
                  colours()[125],  # gunmetal blue - saturated zone
                  colours()[85],   # khaki - unsat zone
                  "tan",    # dried root zone
                  "#A6611A",  # wetted rooT zone
                  "blue")   # surface storage
          # "#018571")  # blue - sat zone (water table)
 # col <- rev(col) # now show the bar the correct way up
  # brewer.pal(5, "BrBG")
  cols <-  rbind(col[1],
                col[2],
                uzcol,  # colour the uz according to amount of storage
                col[4]) #matrix(rep(col, ngroup),nrow=nstores)

#  densities <- rbind(0, 0, round(suzProp), 0)

  rz <- rbind(groups$srz_max-stores$srz,stores$srz)
  # if drying out then zero storage deficit
  drying <- which(stores$wetting==FALSE)
#   if(length(drying)>0)
#   {
#    #    browser("Stores drying out")
#     # if drying then the root zone dries from the top so wet areas are at bottom,
#     # if wetting-up then the base of  the zone wets last. Wett
#     rz[1:2,drying]<- rz[2:1,drying]
#
#     # if the region is "wetting" up then darker shade increases from the top downwards,
#     # if drying out, lighter shade increases from top
#     cols[1:2,drying] <- cols[2:1,drying]
#   }
  # max height of sat zone, including space for "bedrock"
  max.sz <- max(0.1+groups$sd_max)      #   max(rz + groups$sd_max, na.rm=TRUE)
  # excess (overland) storage
  ex <- stores$ex

  # make columns the same height, draw a dotted line at maximum sd
  #  dat<-TRUE(cbind(rz, stores$suz,stores$sd-stores$suz,groups$sd_max-stores$sd))
  dat<- rbind(as.vector(max.sz-groups$sd_max), as.vector(groups$sd_max-stores$sd),
              as.vector(stores$sd), rz, ex)  #  maxh--)


  ylim <- max(c(0,colSums(dat)),na.rm=TRUE)

  widths <- sqrt(groups$area/sum(groups$area, na.rm=TRUE))

  widths[is.na(widths)]<-0.2
  # shade unsaturated zone according to ammount of storage. value gives no. shading
  # lines per inch
  #densities <- rbind(NULL,NULL,20*stores$suz/stores$sd,NULL)
  barplot(dat, col=col,
         # main="Subsurface storages",
          #sub ="Specific storages",
          beside=FALSE,
          las=2,
          space=0,
         # density=densities,
          names.arg=groups$tag,
          xlab ="",
         ylab="",
          width=widths,
         # ylab="Storage (m)",
         cex.main=1, axes=FALSE)
      #    ylim=c(ylim,0))

  mtext(side=2, text="Storage (m)", line=0)
 # axis(side=2, at=pretty(c(0, max(groups$srz_max))))

#   axis(side=2,
#        at=pretty(c(max(groups$srz_max), ylim)))  #,
#        labels=pretty(c(max(groups$srz_max), ylim))-max(groups$srz_max))
  # legend in bottom margin,centered vertically
  leg.y <- grconvertY(par("fin")[2] + par("mai")[3]/2, from="inches")  # par("usr")[4]
#leg.y <- par("usr")[4]
  legend(x=0, y=leg.y,
         ncol=4,
         bty="n",
      #   bg="white",
         xpd=TRUE,
         legend=c("Sat. RZ", "Unsat RZ", "Unsat Zone", "Max SD"),cex=0.8, fill=col)
}


# returns the input ts subsetted by a time range
# either bound or neither can be supplied, if a bound is not supplied then the corresponding bound of the
# input series is used
# dt = time step in hours. If the data intervals are greater than these then
# disaggregate or aggregate if they are larger
GetTimeSeriesInputRange <- function(series, start, endt,
                                    cap="rainfall", cols=1,
                                    dt=1, verbose=TRUE)
{
    if(is.null(series)){return(series)}
    #if(length(series)==0){browser()}
    # subset of columns if specified
    if(length(which(is.finite(series[])))==0){return(series)}
    series <- series[,cols]
    res <- series[index(series)>=start & index(series)<=endt]
    nas <- which(is.na(res))
    if(length(nas)>0)
    {
        if(verbose==TRUE)
        {
            cat(paste(length(nas), " ", cap, " NA data encountered: replaced with 0\n"))
        }
       # res[nas] <- 0
    }

    if(verbose==TRUE)
    {
        # Determine if this value is in mm or m by looking at magnitude
        # 1m of rainfall or pe is impossible so divide through by 1000!
        if(max(res,na.rm=TRUE)>10)
        {
            message("Large values encountered in series: are data in mm/hr?")
            #res <- res /1000
        }
        msg <- paste("Using ", length(res), " ", cap, " observations from ", start, " to ", endt, sep="")
        cat(msg, "\n")
    }
    return(xts(res))
}

# make lists of time series for each hru and flux
collect_flux_ts <- function(groups, fluxes, tms)
{
    catch.area <- sum(groups$area)
    # convert fluxes to a named list
    fluxes <- apply(fluxes, MARGIN=3, function(x){list(x)})
    fluxes <- lapply(fluxes, function(x)x[[1]])
    names(fluxes) <- c("qbf", "qin", "uz", "rain", "ae", "ex", "qof")
    fluxes <- lapply(fluxes,
                     function(flux)
                     {
                         flux <- flux[1:length(tms),]
                         xts(cbind(flux, "mean"=rowSums(flux*groups$area)/catch.area),
                             order.by=tms)
                     })

    return(fluxes)
}


collect_storage_ts <- function(groups, storages, tms)
{
    catch.area <- sum(groups$area)

    storages <- apply(storages, MARGIN=3, function(x){list(x)})
    storages <- lapply(storages, function(x)x[[1]])
    names(storages) <- c("srz", "suz", "sd")


    storages <- lapply(storages,
                       function(storage)
                       {
                           storage <- storage[1:length(tms),]
                           xts(cbind(storage, "mean"=rowSums(storage*groups$area)/catch.area),
                               order.by=tms)

                       })
    return(storages)

}
