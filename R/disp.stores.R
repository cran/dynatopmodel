disp.stores <-
function(groups,stores,ichan=1, nlev=5)
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
  max.sz <- max(0.1+groups$sd_max)      #   max(rz + groups$sd_max, na.rm=T)
  # excess (overland) storage
  ex <- stores$ex

  # make columns the same height, draw a dotted line at maximum sd
  #  dat<-t(cbind(rz, stores$suz,stores$sd-stores$suz,groups$sd_max-stores$sd))
  dat<- rbind(as.vector(max.sz-groups$sd_max), as.vector(groups$sd_max-stores$sd), 
              as.vector(stores$sd), rz, ex)  #  maxh--)
  

  ylim <- max(c(0,colSums(dat)),na.rm=T)

  widths <- sqrt(groups$area/sum(groups$area, na.rm=T))

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
         cex.main=1, axes=F)
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
         xpd=T,
         legend=c("Sat. RZ", "Unsat RZ", "Unsat Zone", "Max SD"),cex=0.8, fill=col)
  #browser()
}
