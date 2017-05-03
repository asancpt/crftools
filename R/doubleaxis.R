set.seed(2015-04-13)
d = data.frame(x =seq(1,10),
               n = c(0,0,1,2,3,4,4,5,6,6),
               logp = signif(-log10(runif(10)), 2))

doubleaxisplot <- function(d, x, logp, n){
	# Axis 1, left
	par(mar = c(5,5,2,5))
	with(d, plot(x, logp, type="l", col="red3", 
				 ylab=expression(-log[10](italic(p))),
				 ylim=c(0,3)))

	# Axis 2, right
	par(new = T)
	with(d, plot(x, n, pch=16, axes=F, xlab=NA, ylab=NA, cex=1.2))
	axis(side = 4)
	mtext(side = 4, line = 3, 'Number genes selected')
	legend("topleft",
		   # Legend
		   legend=c(expression(-log[10](italic(p))), "N genes"),
		   lty=c(1,0), pch=c(NA, 16), col=c("red3", "black"))
}
