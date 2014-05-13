disp.discharges <-
function (title= "DynaTOPMODEL run", runid="", 
                               Qr, 
                               evap, rain, tm, 
                               groups, qobs=NULL, wb=NULL, eff=NULL,
                               ymax=0,
                               disp.par,
                               run.par,           
                               # size of display window (big means show entire simulation period)
                               timeBuffer= 200*24,  # width of display window in hours (defaults to around 4months)
                               ichan=1,
                               units.discharge="mm/hr",...
                              )
{
  cex.axis <- disp.par$"cex.axis"
  par("cex.axis"=cex.axis)
  # move axis title 
  par("mgp"=c(2,1,0))
  # add more space to right for axis titles, trim left margin a bit and shrink bottom margin 
 # par("mai"=c(0.5, 1, 0.5412, 0.75))
  par("xpd"=F)
  # plot a moving window of two weeks onto the current part of the simulation . assume buffer in no. hours
  # rainfall and evapotranspiration on same plot (conversion to seconds for POSix time)
  start  <- max(c(tm-timeBuffer*3600/2, start(Qr)))
  end <- max(c(start+timeBuffer*3600, end(Qr)))
  if(end>end(rain)){
    end<- as.POSIXct(end(rain))
  }
  # inverted plot of rainfall
  # reversed limits, plus a buffer
  ylim2 <- c(max(c(2*rain),na.rm=TRUE),0)  
  
  # everything in mm/hr
  rain <- GetTimeSeriesInputRange(rain, start, end, verbose=FALSE) 
 qobs <- GetTimeSeriesInputRange(qobs, start, end, verbose=FALSE) 
  evap <- GetTimeSeriesInputRange(evap, start, end, verbose=FALSE)  
  qriv <- GetTimeSeriesInputRange(Qr, start, end, verbose=FALSE)    # simulated flows 
  fm <- "%d-%m-%y %H:%m"

  title <- title

  if(tm>start & tm<end)
  {
    timeStr <- format(tm, "%Y-%m-%d %H:%M")  
    # indicate time of latest simulation
  }
  else
  {
    timeStr <- ""
     # paste("Current simuation time: ", format(start, "%Y-%m-%d"), "-", format(end, "%Y-%m-%d"))
  }
 #max(qobs[], na.rm=T),
 # calculate y limits from maximum of evap, observed flows plotted on LH axis
 ylim <- c(0, max(c(0.01,  max(evap,na.rm=T), ymax), na.rm=T))  # evap

# par(new=T)
 disp.rain(rain, disp.par)

  #par(col="black")  
 # par("las"=2)
 # browser()

 
#   plot.zoo(rain, ylim=ylim2, type="h",
#            ylab="",  
#            lwd=2,  
#            xaxt="n",
#            yaxt="n", cex.main=1,bty="n",
#            xlab="",
#            cex.axis=cex.axis,
#            cex.lab=cex.axis)
  if(tm>start & tm<end)
  {
    # indicate time of latest simulation
    abline(v=end(qriv), col="red",lwd=2)
  }
 
  title(main=title, cex.main=cex.axis, line=3)  
  title(sub=timeStr, cex.sub=cex.axis, line=1)
  
  #title(main=title, sub=timeStr)  
  # bottom axis
  locns <- axTicks(3)        
  xlim <- c(start, as.POSIXct(end))  
  par(new=T)   

  DisplayObservedFlows(qobs, start, end, xlim, ylim)   
  par(new=T)   
  # actual / potential evapotranspiration
  plot.zoo(evap, type="h", ylim=ylim, col="wheat",
           xaxt="n", 
           yaxt="n", main="", ann=F, lty=1, lwd=2, ylab="",xlab="",...)
  try(title(line=2,...))
  par(new=T) 
# par(cex=1)
# plot simulated discharges. ensure range matches rain and other data
  disp.qsim(qriv, xlim=xlim, ylim=ylim, disp.par)
  # simulated discharge
#   plot.zoo(qriv, main="", plot.type="single", 
#            xlim=xlim, 
#            ylim=ylim, 
#            cex.axis=1, #cex.axis,
#   				 cex.lab=cex.axis,
#    #        cex.lab=cex.axis,
#            ylab="Specific discharge / evap. (mm/hr)",
#            col="blue", lwd=1, 
#            #ann=FALSE, 
#            xaxt="n", bty="n", xlab="")    
 # par(las=2)

  # actual evap - now non-inverted and histogram

  #grid(col="slategray", nx=NA)
  #  horz axis above with perpendicular labels - more ticks than default- compute tick locations   
  xaxp <- par("xaxp")
  xaxp[3]<-1  # tick label interval

  par("xaxp"=xaxp)
 
 # time axis at top, add formatted labels and lines
 # more detail. get pretty breaks based on day numbers (date-times in seconds from t.origin)
  at <- 24*3600*pretty(xaxp[1:2]/3600/24)
  labs <- as.POSIXlt(at, origin=t.origin()) #, format="%Y-%m-%d")  # replace with graphics$short.time.fmt- 
  labs <- format(labs, format="%d-%b-%y") 

  axis(side=3, at = at, labels=labs, las=1)
  abline(v=at, col="slategray", lty=2)
 
 add.legend(qobs, rain, qriv, disp.par) 
}
