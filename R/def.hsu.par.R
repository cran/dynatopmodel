def.hsu.par <-
function()
{
  list("gauge.id"=1,
  	"srz_max"=0.1, 
       "ln_t0"=7,     # saturation transmissivity
       "m"=0.01, 
       "srz0"=0, "td"=1, 
       "vchan"=1000,
       "vof"=100,
       "k0"=1e8,      # surface conductivity - not used in DynaTM. Used in infiltration excess calcs in Buytaert's implementation of TOPMODEL. Set to large value for no infiltration excess
       "CD"=0.1,      # capillary drive (see Morel-Seytoux and Khanji, 1974, Beven, 1984), as above. 
       "atb.bar"=0,  # areal average of topographic index
       "sd_max"=0.5)    # max storage deficit 
}
