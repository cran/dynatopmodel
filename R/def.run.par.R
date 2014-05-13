def.run.par <-
function(){
  list(
  # "start"=,  # start time / dat
  #  "end"=index(rain[length(rain)]),
  "sim.delay"=10,    # hours to run in order to "bed in" model 
  "debug"=TRUE,         # debug mode (applies breakpoints etc)    			   			                
  "unsat.evap" = F,     # whether evapotranspiration takes place from unsat zone at maximum allowable rate  (see Beven 2012)
  "id" = "DTM",
  "start" = "2000-01-01",
  "end" = "2001-12-13",
 "log.out"="drm.log")		# location for logging output
}
