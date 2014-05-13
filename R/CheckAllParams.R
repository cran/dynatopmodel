CheckAllParams <-
function(disp.par, run.par, warn="stop")
{
  CheckParams(obj = disp.par, 
              names=c("dem",
                      "cm",                         
                      "text.out",  # where to send text output - could be a file or console                   	
                      "graphics.delay",     	# wait before display, expresses in time steps
                      "graphics.show",   #  show graphic output  
                      "graphics.interval",   # graphics display interval, in time steps
                      "graphics.out",    # save location
                      "graphics.save",    # save location                       	
                      "spatial.interval"),
                      warn=warn)
  
  CheckParams(obj = run.par, 
              names=c("title",
                      "start", 
                      "end",
                      "sim.delay",    # hours to run in order to "bed in" model 
                      "debug",         # debug mode (applies breakpoints etc)					   			                
                      "run.out", #         
                      "id",
                      "log.out"),
                      warn=warn)
}
