#############################################
#Simulate from ICAR, est. cov. from Matern
#############################################

pts = cbind(rep(1:50,50),rep(1:50,each=50))
plot(pts,pch=19,cex=.7)
abline(h=((0:10)*5+.5),v=((0:10)*5+.5))

D = as.matrix(dist(pts))/49

Sig = exp(-D)+diag(1e-6,50^2)
cSig = chol(Sig)

yobs = c(t(cSig)%*%matrix(rnorm(50^2),ncol=1))
library(fields)
image.plot(x=1:50,y=1:50,z=matrix(yobs,50,50))
points(pts,pch=19,cex=.5)
abline(h=((0:10)*5+.5),v=((0:10)*5+.5))

#code 100x2500 K matrix
# K is basically an indicator matrix to identify which sub-poins correspond to which general block
K = matrix(0,100,2500)
blocks = cbind(rep(1:10,10),rep(1:10,each=10))

for(i in 1:100){
  K[i,] = pts[,1] >= blocks[i,1]*5-4 & pts[,1] <= blocks[i,1]*5 & pts[,2] >= blocks[i,2]*5-4 & pts[,2] <= blocks[i,2]*5
}

y_block = K%*%yobs/25

# Plot the fine grid, and blocked grid, side-by-side
par(mfrow=c(1,2))
image.plot(x=1:50,y=1:50,z=matrix(yobs,50,50))
image.plot(x=1:10,y=1:10,z=matrix(y_block,10,10),zlim=range(yobs))
par(mfrow=c(1,1))
#points(pts[K[20,]==1,],col='red',pch=19)
#points(pts[1:20,],col='blue',pch=19)

#############################################
#create the covariance for the averaged model
#############################################
cov.upper = matrix(NA,10,10)
#tri = numeric()
image.plot(x=1:50,y=1:50,z=matrix(yobs,50,50))
for(i in 1:10){
  for(j in i:10){
    cov.upper[i,j] = sum(exp(-(D[K[1,]==1,K[(i-1)*10+j,]==1])^1))/sum(K[1,])^2
    # Plot to ensure it is covering the parts desired
    print(points(pts[K[(i-1)*10+j,]==1,],col='blue',pch=19))
  }
}

cov.block = matrix(NA,100,100)
#block_distances = array(NA,c(100,100,2))
for(i in 1:100){
  for(j in i:100){
    ind = sort(abs(blocks[i,]-blocks[j,]))+1
    cov.block[i,j] = cov.upper[ind[1],ind[2]]
    cov.block[j,i] = cov.block[i,j]
  }
}
############################
#log-likelihood
##############################

cCB = chol(cov.block+diag(1e-6,100))
library(mvtnorm)
dmvnorm(c(y_block),sigma=cov.block,log=TRUE)

################################
#function it
################################
library(rSPDE)
library(mvtnorm)

setup = function(a_res,c){
  l_res = a_res*c # Total Latent length of one side
  l_pts = cbind(rep(1:l_res,l_res),rep(1:l_res,each=l_res)) # Latent Grid
  a_pts = cbind(rep(1:a_res,a_res),rep(1:a_res,each=a_res)) # Aerial Grid
  
  l_D = as.matrix(dist(l_pts))/(l_res-1) # Not too sure what this is doing. I know it's supposed to be finding the distances, but from what to what?

  # K is basically an indicator matrix showing which indices of the latent grid correspond to which index of the aerial grid
  K = matrix(0, a_res^2,l_res^2)
  for(i in 1:a_res^2){
    K[i,] = l_pts[,1] >= a_pts[i,1]*c-(c-1) & l_pts[,1] <= a_pts[i,1]*c & 
      l_pts[,2] >= a_pts[i,2]*c-(c-1) & l_pts[,2] <= a_pts[i,2]*c
  }
  K = K/c^2

  return(list(K=K,l_D=l_D,a_pts=a_pts,l_pts=l_pts))
}


# Matern Covariance Function: https://en.wikipedia.org/wiki/Mat%C3%A9rn_covariance_function
mk_cov = function(s2,rho,nu,setup){

  d <- setup$l_D # Distance between two points, pull from setup 
  # Version I started before find the function: s2 * 2^(1-nu) / gamma(nu) * (sqrt(2*nu) * d / rho)^nu
  kappa <- sqrt(2*nu)/rho
  # Function reference: https://search.r-project.org/CRAN/refmans/rSPDE/html/matern.covariance.html
  matern.covariance(d, kappa, nu, sqrt(s2))
}

get_likelihood <- function(log_params, y_block, setup) {
  # Enforce them being positive
  s2 <- exp(log_params[[1]])
  rho <- exp(log_params[[2]])
  nu <- exp(log_params[[3]])
  mat <- mk_cov(s2, rho, nu, setup)
  sig <- setup$K %*% mat %*% t(setup$K)
  dmvnorm(c(y_block), sigma=sig, log=TRUE)
}


################################
# Test Script
################################

# Initialize dimensions
s2 <- 4
rho <- 2
nu <- 2
aerial_blocks <- 2
lat_per_dim <- 2
tot_lat_len <- aerial_blocks * lat_per_dim
test <- setup(aerial_blocks, lat_per_dim)
# Generate sample data
Sig = mk_cov(s2, rho, nu, test)#+diag(1e-6,tot_lat_len^2)
cSig = chol(Sig)
yobs = c(t(cSig)%*%matrix(rnorm(tot_lat_len^2),ncol=1))
y_block = test$K%*%yobs/(tot_lat_len/2)

# Initial parameters: s2, rho, nu
params <- c(5, 2, 2)

# See if it will return the same parameters I start with
# Root find
res <- optim(params, get_likelihood, y_block = y_block, setup = test)
res$convergence
# The likelihood roots over the log params, so convert to regular scale
exp(res$par)




# Try fixing nu and just search for sigma and rho
# Try implemeitng the gradient descent newton-raphson




# yobs_block = c(t(cCB)%*%matrix(rnorm(10^2),ncol=1))

# image.plot(1:10,1:10,matrix(yobs_block,10,10))

# points(blocks[1:15,],col='blue')


# X = matrix(rnorm(1e8),1e4,1e4)
# A = matrix(rnorm(1e6),1e2,1e4)

# AXA = A%*%X%*%t(A)