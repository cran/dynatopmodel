disp.chan.stores <-
function(groups, stores, ichan)
{
	chan.stores <- stores[ichan,]
	chan.groups <- groups[ichan,]
	nchan <- length(ichan)
	# scale to percentage?
	chan.groups$sd_max <-0.01
	dat <- rbind(chan.groups$sd_max-chan.stores$sd, chan.stores$sd)
	cols <- rbind(rep("blue", nchan), rep("white", nchan), names.arg = chan.groups$id)
	barplot(horiz=T, dat, col=cols, xlab="Average depth (m)")
	
}
