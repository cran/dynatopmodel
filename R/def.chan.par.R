def.chan.par <-
function(vals=list())
{
  res <- list("srz_max"=0, # rainfall goes straight to the unsat zone infiltration
       "ln_t0"=8, 
       "m"=0.01, 
       "srz0"=0,
       "td"=1e-8 , 
       "vchan"=1500,
       "vof"=10,
       "sd_max"=2)    # depth of rectangular channel?
  merge.lists(res, vals)
}
