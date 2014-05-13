def.disp.par <-
function()        
{
  list("title.main"= "Dynamic TOPMODEL",         
    "title.sub"= "",         
    "legend.show"=T,
    "text.out"="",  # where to send text output - could be a file or console (blank) 
    "time.fmt"="%d-%b-%Y %H:%M",
    "eff.measure"="",     # calculate and show efficiency
    "graphics.window.length"=120*24,      # length of display window (hr)
    "graphics.delay"=10,       # wait before display, expressed in time steps
    "graphics.show"=TRUE,   #  show graphic output  
    "graphics.spatial.show"=FALSE,   #  show graphic output 
    "graphics.spatial.output"="qbf", # what to show
    "graphics.interval"=12,   # graphics display interval, in time steps (hrs)
    "graphics.out"= ".",    # save location
    "graphics.save"= F,    # whether to save graphics 
    "graphics.save.width"=1024,  # dims of output window, pixels
    "graphics.save.height"=768,
    "graphics.spatial.window.id"=0,
    "graphics.spatial.3d" = T,   # plot catchment deficit data in 3d
    "graphics.spatial.theta"=45,  # view angles
    "graphics.spatial.phi"=30,
    "graphics.spatial.max.percent" =25,  # max val in % to be coloured in spatial view
    "graphics.spatial.expand"=0.5,  # vertical expansion factor for 3d view
    "graphics.save.interval"= 48,  # number of time steps between saving  
    "cex.axis"=1,          # expansion factor applied to plot axes titles and labels
    "graphics.fn.fmt"="%Y%m%d-%H.jpg",  # format for filenames for saved graphics
    "spatial.interval"=100)
}
