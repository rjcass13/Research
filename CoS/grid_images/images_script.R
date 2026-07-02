# Dimensions of the Aerial grid
nbw <- 4 # N blocks wide
nbh <- 4 # N blocks tall
# Dimensions of the Sub-grid
spw <- 5 # N points wide
sph <- 5 # N points tall

total_width <- spw*nbw
total_height <- sph*nbh

pts <- cbind(rep(1:total_width,total_height)-.5,rep(1:total_height, each=total_width)-.5)

# Aerial view
plot(pts,pch=19,cex=.7, xlim = c(0, total_width), ylim = c(0, total_height), col = 'white', 
  main = 'Grid of Aerial blocks', xlab = 'X', ylab = 'Y')
abline(h=((0:nbh)*sph),v=((0:nbw)*spw), lwd = 2)
text(spw/2, sph*(nbh - .5), expression("A"[i]), cex = 5)
cell_top <- total_height
cell_bot <- total_height-spw
cell_right <- spw
lines(c(0, cell_right, cell_right, 0, 0), c(cell_bot, cell_bot, cell_top, cell_top, cell_bot), col = 'red', lwd = 5)


# Aerial view with sub-points
plot(pts,pch=19, xlim = c(0, total_width), ylim = c(0, total_height), col = 'blue', 
  main = 'Grid of Sub-points', xlab = 'X', ylab = 'Y')
abline(h=((0:nbh)*sph),v=((0:nbw)*spw), lwd = 2)
lines(c(0, cell_right, cell_right, 0, 0), c(cell_bot, cell_bot, cell_top, cell_top, cell_bot), col = 'red', lwd = 5)


# Zoomed in on cell
sub_points_ex <- cbind(rep(1:spw,sph)-.5,rep((cell_bot+1):cell_top, each=spw)-.5)
plot(sub_points_ex[-1,],pch=19, xlim = c(0, cell_right), ylim = c(cell_bot, cell_top), col = 'blue', 
  main = 'Zoomed in Single Aerial Block', xlab = 'X', ylab = 'Y')
lines(c(0, cell_right, cell_right, 0, 0), c(cell_bot, cell_bot, cell_top, cell_top, cell_bot), col = 'red', lwd = 5)
text(sub_points_ex[1,1], sub_points_ex[1,2], expression("S"["i,j"]), cex = 3)


