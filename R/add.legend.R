add.legend <-
function(qobs, rain, qsim, disp.par, nrow=2)
{
  if(disp.par$legend.show)
  {
    #cols <- get.qobs.cols(qobs)  
    titles <- c(paste("Simulated: ", colnames(qsim)), 
                paste("Rain:", colnames(rain)), 
                paste("Observed:", colnames(qobs)), 
                "Evapotranspiration")
       
    cols <- c(qsim.cols(qsim), 
              rain.cols(rain), 
              get.qobs.cols(qobs), "wheat")
    #  determine plot limits
    xlim <- par("usr")[1:2]
    # xjust and yjust controls how legend justified wrt x and ycoord: 2=right / top justified (doc is wrong)
    legend(x=xlim[2], y=-0.01, legend=titles, #ncol=length(titles), 
           xpd=T,  # needed in order to plot outside figure margin
           yjust=2, xjust=1,
           ncol=max(2, round(length(titles)/nrow+0.5)),
           fill=cols,  cex=0.9) #, bg="white")  
    #bg="#FFFFFFCC")
  }

}
