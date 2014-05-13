compassRose <-
function(x="topleft",y=NULL,rot=0,cex=1, col="white", pos="topleft")
{	
	oldcex<-par(cex=cex)
	mheight<-strheight("M")
	xylim<-par("usr")
	

	plotdim<-par("pin")
	xmult<-(xylim[2]-xylim[1])/(xylim[4]-xylim[3])*plotdim[2]/plotdim[1]
	point.angles<-seq(0,7*pi/4,by=pi/4)+pi*rot/180
	crspans<-rep(c(mheight*3,mheight/2),4)
	
	r <- max(crspans) /2
	ynec <-	max(crspans) + 2*strheight("W")
	xnec <-	max(crspans)*xmult + 2*strwidth("W")
	# if position not supplied then place in the TL corner by default
	
	if(x=="topleft")
	{
		x <- xylim[1] + xnec
		# top
		y <- xylim[4] - ynec		
	}
	else if(x=="topright")
	{
		x <- xylim[2] - xnec
		y <- xylim[4] - ynec
	}
	else if(x=="bottomleft")
	{
		x <- xylim[1] + xnec
		y <- xylim[3] + ynec		
	}	
	else if(x=="bottomright")
	{
		x <- xylim[2] - xnec
		y <- xylim[3] + ynec			
	}
	
	
	#	browser()
	
	xpoints<-cos(point.angles)*crspans*xmult+x
	ypoints<-sin(point.angles)*crspans+y
	
	shape::filledcircle(r1 = r, r2 = r*0.8, mid = c(mean(xpoints),mean(ypoints)), col="white", lcol="black")	
	#	browser()
	
	
	polygon(xpoints,ypoints, col=col)
	
	txtxpoints<-cos(point.angles[c(1,3,5,7)])*1.33*crspans[1]*xmult+x
	txtypoints<-sin(point.angles[c(1,3,5,7)])*1.33*crspans[1]+y
	text(txtxpoints,txtypoints,c("E","N","W","S"))
	par(oldcex)
}
