pEvap <-
function(dt=1, dStart=1, numDays=365, eMin=0, eMax=0)
{
  # number days, starting at 0
  days <- (dStart-1):(numDays+dStart-1)
	
	# ET curve has period of 365, at x=0 and x=364 ET = eMin 
	fact <- 1+sin(2*pi*days/365-pi/2)
	# fact goes from 0 at dStart to 2 at day = 365/2
	# calculate daily total ET
	DET <- eMin + 0.5*(eMax-eMin)*fact

	# calculate, start, length and end of all days using sine curve above
  dawn <- 10 - 2.5*fact
  dayLength <- 6 + 4*fact
  sunDown = dawn + dayLength
  dayTotET <- DET

	# if required calculate PE at times steps within day
	# assume no ET outside daylight hours, form is sine curve peaking at 
	# midday with total area is equal to DET for this day calculated above
  # divide days into stretches of length dt hours 

  hrs <- 1:(24%/%dt)*dt

  DET<- array()
  for(day in 1:numDays)
  {
    # proportion of daylight hours passed 
    fract <- (hrs-dawn[day])/dayLength[day] 
    # no ET before dawn
    fract[fract<0] <- 0

    # If: 
    # T = day total ET
    # l = day length
    # h = hours since dawn
    # f = proportion of day passed since dawn
    # then we have
    # e(h) = 0.5*pi*T/l . sin(pi*h/l) or
    # e(f) = 0.5*pi*T/l . sin(pi*f)
    # total e.t. from dawn to each sample time is
    # 0.5*T*(1-cos(pi*f))
    cE <- -cos(pi*fract)  # omit factor
    # same array shifted right by 1 and cos(0) inserted
    cE2 <- c(-1,cE[1:length(cE)-1])
    # e.t. in interval is difference between cumulative e.t.s 
    e <- 0.5*dayTotET[day]*(cE-cE2)

    # deal with values outside daylight hours
    e[which(fract>1)] <- 0  
    # add to result
    DET <- c(DET, e)
  }

	return(DET)
}
