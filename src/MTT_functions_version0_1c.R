
# defines the different transfer functions

GM<-function(inputseries,x,par1,par3,par4){
  beta<-par1/par3
  gamma <- dgamma(x, shape=par3, scale = beta, log = FALSE) #make gamma distribution at each location pareter x
  # g(tau)
  g<-gamma/sum(gamma)#g(tau) is technically a weighting term with the sum of 1. this command also transponses the gtau
  if (par4==0) {g=g} else {g=c(matrix(data=0,ncol=par4),g);length(g)=length(x)} # adds a piston-flow component if defined by the user. 
  if (decay==1) {g=g*decay.vec} #gtau with decay losses
  #z<-conv(g,inputseries) #function from the signal package(Juliens uses that for the gamma function)
  z<-convolve(g,inputseries,conj=FALSE)
  z<-z[1:length(x)]
  if (decay==1) {g=g/decay.vec} #restores gtau without decay losses for saving
  gamma=list(z,g)

}

EM<-function(inputseries,x,par1,par4){
  exp <- dexp(x, rate = 1/par1, log = FALSE) #make exponential distribution at each location pareter x
  # g(tau)
  g<-exp/sum(exp)#gtau is technically a weighting term with the sum of 1. this command also transponses the gtau 
  if (par4==0) {g=g} else {g=c(matrix(data=0,ncol=par4),g);length(g)=length(x)} # adds a piston-flow component if defined by the user.
  if (decay==1) {g=g*decay.vec} #gtau with decay losses
  #z<-conv(g,inputseries) #function from the signal package(Juliens uses that for the gamma function)
  #z<-convolve(t(g),rev(inputseries),conj=FALSE,type =c("circular")) #actually the wrong type of convolution
  z<-convolve(g,inputseries,conj=FALSE)
  z<-z[1:length(x)]
  if (decay==1) {g=g/decay.vec} #restores gtau without decay losses for saving
  exponential=list(z,g)
  
}


DM<-function(inputseries,x,par1,par3,par4){
  g<-(4*pi*par3*x/par1)^(-0.5)*x^-1*exp(-(1-(x/par1))^2*(par1/(4*par3*x)))
  if (par4==0) {g=g} else {g=c(matrix(data=0,ncol=par4),g);length(g)=length(x)} # adds a piston-flow component if defined by the user.
  if (decay==1) {g=g*decay.vec} #gtau with decay losses
  #z<-conv(g,inputseries) #function from the signal package(Juliens uses that for the gamma function)
  #z<-convolve(t(g),rev(inputseries),conj=FALSE,type =c("circular")) #actually the wrong type of convolution
  z<-convolve(g,inputseries,conj=FALSE)
  z<-z[1:length(x)]
  if (decay==1) {g=g/decay.vec} #restores gtau without decay losses for saving
  dispersion=list(z,g)
}


DEM<-function(inputseries,x,par1,par3,par4){
  exp <- dexp(x, rate = 1/par1, log = FALSE) #make exponential distribution at each location pareter x
  # g(tau)
  g1<-exp/sum(exp)#gtau is technically a weighting term with the sum of 1. this command also transponses the gtau
  exp <- dexp(x, rate = 1/t_dem, log = FALSE) #make exponential distribution with a mean transit time defined by the user
  g2<-exp/sum(exp)#gtau is technically a weighting term with the sum of 1. this command also transponses the gtau
  g <- (g1*(1-par3)+g2*par3) # sums both transit time distributions weighted by a factor whose sum is one.
  if (par4==0) {g=g} else {g=c(matrix(data=0,ncol=par4),g);length(g)=length(x)} # adds a piston-flow component if defined by the user.
  if (decay==1) {g=g*decay.vec} #gtau with decay losses
  #z<-conv(g,inputseries) #function from the signal package(Juliens uses that for the gamma function)
  #z<-convolve(t(g),rev(inputseries),conj=FALSE,type =c("circular")) #actually the wrong type of convolution
  z<-convolve(g,inputseries,conj=FALSE)
  z<-z[1:length(x)]
  if (decay==1) {g=g/decay.vec} #restores gtau without decay losses for saving
  dexponential=list(z,g)
  
}


