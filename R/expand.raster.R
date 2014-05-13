expand.raster <-
function(rast, fact, keep.origin = T, crs=rast@crs)
{
   vals <- rast[]
   
   ext <- extent(rast)
   # scales the extent around original centre
   ext2 <- ext*fact
   
   rast2 <- raster(ncol=ncol(rast), nrow=nrow(rast),ext=ext2, 
                   #xmn=ext2@xmin, xmx=ext2@xmax, ymn=ext2@ymin, ymx=ext2@ymax,
                   crs=crs) 
   
   # reestablish origin
   if(keep.origin)
   {
     rast2<-shift(rast2, x=ext@xmin-ext2@xmin, y=ext@ymin-ext2@ymin)  
   }
   rast2 <- raster::setValues(rast2, getValues(rast))
   return(rast2)
}
