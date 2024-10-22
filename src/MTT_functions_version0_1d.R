
# defines the different transfer functions

EM<-function(inputseries,x,par1,par4){ # exponential model

  Q_mean <- mean(Q) #mean discharge
  Vm <- par1*Q_mean # volume in storage as the product of the mean transit time and the mean discharge
  ci=inputseries # input concentration
  dQdt <- c(diff(Q)[1],diff(Q)) # computes the dQ/dt term. The first entry is the difference between the discharge at t1 and at t2.
  Qin=td*dQdt+Q # influx into the system calculated as a function of the ouflow and the dynamic turnover time of the variable part
  phi=Qin*ci # input flux
  zi <- Q/(Vm+Q*td)
  d <- length(x)
  mat.CI <- matrix(data=0,nrow=d,ncol=d)
  for (i in 1:d)  {
      z <- cumsum(zi[i:d])
      CI <- exp(-z)
      if (par4==0) {CI <- CI} else {CI <- c(matrix(data=0,ncol=par4),CI);length(CI)=d+1-i} # adds a piston-flow component if defined by the user.
      if(exists("index_datesel") & i==index_datesel) {CI.out <- 0*vector(mode="numeric",length=d); CI.out[1:(d-i+1)] <- CI} # saves the transit time distribution of a chosen date is selected by the user
      #CI <-  (1/par1)*exp(-x/par1) 
      #CI <-  CI[i:d]
      if (decay==1) {CI=CI*decay.vec[1:(d-i+1)]} #weighting function for time "i" with decay losses
      mat.CI[i:d,i]=CI # adds the weights for time "i" to the matrix comprising all weights for all times.
  }

  mult <- mat.CI%*%phi # matrix vector multiplication replacing the convolution function

  z<-mult[1:length(x),1]/(td*Q+Vm)
  exponential=list(z,CI)
  if(exists("index_datesel"))   exponential=list(z,CI.out) # if a date has been selected, the corresponding transit time distribution is saved.

}

 

DM<-function(inputseries,x,par1,par3,par4){ # dispersion model

  Q_mean <- mean(Q) #mean discharge
  Vm <- par1*Q_mean # volume in storage as the product of the mean transit time and the mean discharge
  ci=inputseries # input concentration
  dQdt <- c(diff(Q)[1],diff(Q)) # computes the dQ/dt term. The first entry is the difference between the discharge at t1 and at t2.
  Qin=td*dQdt+Q # influx into the system calculated as a function of the ouflow and the dynamic turnover time of the variable part
  phi=Qin*ci # input flux
  zi <- Q/(Vm+Q*td)
  d <- length(x)
  mat.CI <- matrix(data=0,nrow=d,ncol=d)
  for (i in 1:d)  {
      z <- cumsum(zi[i:d])
      CI <- ((1/par3)*(4*pi*z^3))^(-0.5)*exp(-(1/par3)*(1-z)^2/(4*z))
      if (par4==0) {CI <- CI} else {CI <- c(matrix(data=0,ncol=par4),CI);length(CI)=d+1-i} # adds a piston-flow component if defined by the user.
      if(exists("index_datesel") & i==index_datesel) {CI.out <- 0*vector(mode="numeric",length=d); CI.out[1:(d-i+1)] <- CI} # saves the transit time distribution of a chosen date is selected by the user
      #CI <-  (1/par1)*exp(-x/par1) 
      #CI <-  CI[i:d]
      if (decay==1) {CI=CI*decay.vec[1:(d-i+1)]} #weighting function for time "i" with decay losses
      mat.CI[i:d,i]=CI # adds the weights for time "i" to the matrix comprising all weights for all times.
  }

  mult <- mat.CI%*%phi # matrix vector multiplication replacing the convolution function

  z<-mult[1:length(x),1]/(td*Q+Vm)
  dispersion=list(z,CI)
  if(exists("index_datesel"))   dispersion=list(z,CI.out) # if a date has been selected, the corresponding transit time distribution is saved.

}

