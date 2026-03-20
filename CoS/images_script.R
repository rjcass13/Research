# Dimensions of the Aerial grid
nbw <- 2 # N blocks wide
nbh <- 2 # N blocks tall
# Dimensions of the Sub-grid
spw <- 5 # N points wide
sph <- 5 # N points tall


pts = cbind(rep(1:spw,sph)-.5,rep(1:sph,each=spw)-.5)
plot(pts,pch=19,cex=.7, xlim = c(0, nbw*spw), ylim = c(0, nbh*sph))
abline(h=((0:nbh)*sph),v=((0:nbw)*spw), lwd = 2)
text(spw/2, sph*(nbh - .5), expression("A"[i]), cex = spw*sph/(nbw*nbh))
text(rep(1:spw,sph)-.5+spw, rep(1:sph,each=spw)-.5+sph, expression("L"["i,j"]), cex = 1.5)
