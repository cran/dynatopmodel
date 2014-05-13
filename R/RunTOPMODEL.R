RunTOPMODEL <-
function(dem, 
                        riv=raster::setValues(dem, 0),  # raster containing, desults to empty
                        nreach=5, 
                        nbreaks=15, rain,                       
                        params,
                        delays=routing,
                        topidx.frame = topidx.frame,
                        qt0, 
                        dt, 
                        thresh=10.5,
                        et0, qobs=NA, 
                        verbose=T)
{
  # ensure time series objects are cast to vectors and rasters to matrices
  rain[which(is.na(rain))]<-0

  # deal with nas
  et0[which(is.na(et0))]<-0
  
  #qobs <- as.vector(qobs)
  dx <- xres(dem)  
  
   # if(is.null(delays)){delays <- get.routing(dem, nreach=nreach)}
   # construct a routing table if not already present
   if(is.null(delays))
   {
     routing <- get.routing.table(dem, nreach=nreach)
   }
   
  if(!is.numeric(params)){
    stop("All parameters supplied must be numeric")
  }
  paramsTM <- cbind(
    "qs0"=qt0,               # Initial subsurface flow per unit area [m]
    "lnTe"=params[,"ln_t0"],	 # log of the areal average of T0 [m2/h]
    "m"=params[,"m"],	       # Model parameter controlling the rate of decline of transmissivity in the soil profile, see Beven, 1984
    "Sr0"=params[,"srz_max"]*(1-params[,"srz0"]), #	 Initial root zone storage deficit [m] - determine from maximum
    SrMax=params[,"srz_max"],	 # Maximum root zone storage deficit [m]
    td=params[,"td"],	       #Unsaturated zone time delay per unit storage deficit [h/m]
    vch=params[,"vchan"],	             # channel flow outside the catchment [m/h] (currently not used)
    vr=params[,"vchan"],                #	 channel flow inside catchment [m/h]
    k0	= params[,"k0"],      # Surface hydraulic conductivity [m/h]. Set to large value for no infiltration excess calcs
    CD	= params[,"CD"],      # capillary drive, see Morel-Seytoux and Khanji (1974)
    dt = dt)	               # The timestep [hr]

  colnames(paramsTM)<- c("qs0","lnTe","m","Sr0","SrMax","td","vch","vr","k0","CD","dt")
    
# discretisation of catchment according to ATB
  if(is.null(topidx.frame))
  {
    topidx.frame <- disc.topidx(dem, nbreaks, riv)
  }
  # discretize catchment according to Kirkby index
#  topidxs <- topidx(as.matrix(dem), res=dx), river=as.matrix(riv))
#  topidx.frame <- make.classes(as.vector(topidxs$atb), nbreaks)
start.time <- Sys.time()
  # run TOPMODEL
  res <- topmodel::topmodel(parameters = paramsTM, topidx=topidx.frame, 
  								delay=delays, rain= as.vector(rain), 
                  ET0= as.vector(et0), verbose=T)

  mx.len <- min(length(et0), length(res$Ea))
  # construct result in format consistent with dynamic version
  evap <- cbind("pe"=et0[1:mx.len,], "ae"=res$Ea[1:mx.len,])
#  evap <- xts(cbind("pe"=et0, "ae"=Ea), order.by=
  run <- list(qobs=qobs, evap=evap, rain=rain)
  # simulated discharges (specific, as determined by water balance)
  run$qsim <- xts(order.by=index(rain), res$Q)  #/catch.area(dem))
  run$dur <- difftime(Sys.time(), start.time)
  run$storages$sd <- xts(res$S, order.by=index(rain))
  run$fluxes$qbf <- xts(res$qs, order.by=index(rain))
  run$fluxes$qof <- xts(res$qo, order.by=index(rain))
  #run$fluxes$evap <- evapxts(res$qo, order.by=index(rain))
# 

  run$wb <- sum(rain-evap[,"ae"])-sum(res$Q)  # net gain
            
  # net storage gain is -ve deficit gain with the initial root zone storage taken into account
  store.gain <- head(res$S, n=1)-tail(res$S, n=1) #-paramTM$Sr0
  run$wb <- run$wb - store.gain
  
  #run$fluxes <- list(res$)
#  run$storages <- stores
  return(run)
}
