SAS_EFs <- function(Pars, data, c_out) {
  #--------------------------------------------------------------------------
  # prepare the run
  #--------------------------------------------------------------------------
  
  if (example == 1) {
   parQ <- Pars[1]  # SASQ parameters
   parET <- Pars[2]  # SASET parameters
   S0 <- Pars[3]
  }
    
  if (example == 2 || example == 3) {
    parQ <- Pars[1:2]  # SASQ parameters
    parET <- Pars[3]  # SASET parameters
    S0 <- Pars[4]
  }
  
  # set a few constants
  NN <- length(data$J)  # length of the timeseries
  ndistr <- length(data$index_datesel)  # number of age distributions that will be saved
  
  # preallocate variables
  S_T <- numeric(NN)  # rank storage (function of age T)
  C_ST <- numeric(NN)  # rank storage concentration (function of age T)
  C_Q <- numeric(NN)  # stream concentration (function of time t)
  age_matr <- matrix(0, nrow = NN, ncol = ndistr)  # matrix to store the desired age distributions
  
  # initial conditions
  C_Q[1] <- data$C_S0  # initial streamflow concentration equal to the initial storage
  length_s <- 1  # rank storage vector length
  S_T[length_s] <- S0  # initial rank storage [mm]
  C_ST[length_s] <- data$C_S0  # mean concentration of the initial rank storage

  source(paste(functions_dir,"fSAS_pl.R",sep=""))
  source(paste(functions_dir,"fSAS_pltv.R",sep=""))
  source(paste(functions_dir,"fSAS_beta.R",sep=""))
  
  Omega_Q <- do.call(data$SASQName, c(list(S_T[1:length_s] / S_T[length_s], parQ, data$wi[1])))  # [-]
  Omega_ET <- do.call(data$SASETName, c(list(S_T[1:length_s] / S_T[length_s], parET, data$wi[1])))  # [-]
  
  #--------------------------------------------------------------------------
  # MODEL LOOPS
  #--------------------------------------------------------------------------
  
  # let's go
  for (j in 1:(NN - 1)) {
    #------------------------------------------------------------------
    # SOLVE THE AGE BALANCE and evaluate the rank storage concentration
    #------------------------------------------------------------------
    # 0) define the domain for the SAS function evaluation (basically a shifted S_T with new water addition)
    age1 <- max(0, data$dt * (data$J[j] - data$Q[j] * Omega_Q[1] - data$ET[j] * Omega_ET[1]))  # estimate of resident water with age 1
    dom <- (c(0, S_T[1:length_s]) + age1) / (S_T[length_s] + age1)  # rescaled domain for SAS evaluation
    
    # 1) evaluate the SAS functions Omega over the domain 'dom'
    Omega_Q <- do.call(data$SASQName, c(list(dom, parQ, data$wi[j])))  # [-]
    Omega_ET <- do.call(data$SASETName, c(list(dom, parET, data$wi[j])))  # [-]
    
    # 2) solve the master equation balance
    S_T[1:(length_s + 1)] <- pmax(0, c(0, S_T[1:length_s]) + data$dt * data$J[j] - data$dt * (data$Q[j] * Omega_Q + data$ET[j] * Omega_ET), na.rm = TRUE)
    S_T[2:(length_s + 1)] <- pmax(S_T[2:(length_s + 1)], S_T[1:length_s], na.rm = TRUE)

    # 3) update solute concentration for each parcel
    C_ST[2:(length_s + 1)] <- C_ST[1:length_s]  # conservative transport of the elements
    C_ST[1] <- data$C_J[j]  # concentration of the new input
    
    # 4) check if the vectors need to grow or not
    if (j == 1 || S_T[length_s] < data$f_thresh * S_T[length_s + 1]) {  # still need to grow
      length_s <- length_s + 1
    } else {  # oldest element are merged into an old pool #SL: is pmax with na.rm=T correct in this case?
      C_ST[length_s] <- pmax(                                 
       0, (C_ST[length_s + 1] * (S_T[length_s + 1] - S_T[length_s]) +
              C_ST[length_s] * (S_T[length_s] - S_T[length_s - 1])) /
          (S_T[length_s + 1] - S_T[length_s - 1]) # SL: na.rm=T added
      , na.rm = TRUE)  # update mean concentration of the pool
      S_T[length_s] <- S_T[length_s + 1]  # merge the oldest elements of S_T
      Omega_Q[length_s] <- Omega_Q[length_s + 1]
      #Omega_Q[length_s + 1] <- NULL  # merge the oldest values of the Omega functions
      Omega_Q <- Omega_Q[-(length_s + 1)] # SL: <- NULL (see row above) replaced by this 
      Omega_ET[length_s] <- Omega_ET[length_s + 1]
      #Omega_ET[length_s + 1] <- NULL  # merge the oldest values of the Omega functions
      Omega_ET <- Omega_ET[-(length_s + 1)] # # SL: <- NULL (see row above) replaced by this 
    }
    
    #----------------------------------------
    # COMPUTE output: stream concentration
    #----------------------------------------
    # compute discharge age distribution (pQ) and concentration (C_Q)
    pQ <- diff(c(0, Omega_Q))  # [-] this is pQ(T)*dT and it is equivalent to omegaQ(S_T)*dS_T
    #if(decay==1) pQ <- pQ*rev(decay.vec[1:length(pQ)]) # reduces each weight of the pQ object by the corresponding decay in the case of tritium.
    if(decay==1) pQ <- pQ*decay.vec[1:length(pQ)] # reduces each weight of the pQ object by the corresponding decay in the case of tritium. Note that both pQ and decay vectors are arranged in increasing residence times from lowest to highest.
    C_Q[j + 1] <- sum(C_ST[1:length_s] * pQ)  # streamflow modeled concentration
    
    #-------------------------
    # COMPUTE output: other
    #-------------------------
    # for the selected dates, store discharge age distributions in a matrix
    if (any(data$index_datesel - 1 == j)) {
      #age_matr[1:length_s, data$index_datesel - 1 == j] <- pQ
      pQ.out <<- pQ
    }
  }

  C_Q <<- C_Q  
  #--------------------------------------------------------------------------
  # save output
  #--------------------------------------------------------------------------
  
  # option to save some output
  if ("save_output" %in% names(data) && data$save_output == 1) {

    outfilename <- 'results/all_output.rds'  # Choose the output filename
    
    # List the variables that you want to save
    varlist <- list(
      data = data,
      Pars = Pars,
      C_Q = C_Q, # errechneter isotopenoutput
      age_matr = age_matr
    )
    
    # Save selected variables as an RDS file
    saveRDS(varlist, file = outfilename)
    
    #-----------
    # outfolder <- 'results'  # Choose the output folder
    # 
    # # Create the output folder if it doesn't exist
    # if (!file.exists(outfolder)) {
    #   dir.create(outfolder)
    # }
    # 
    # # Save individual variables as CSV files
    # # Save each element of the 'data' list as a CSV file
    # lapply(names(data), function(var_name) {
    #   write.csv(data[[var_name]], file = file.path(outfolder, paste0(var_name, '.csv')), row.names = FALSE)
    # })
    # write.csv(Pars, file = file.path(outfolder, 'Pars.csv'))
    # write.csv(C_Q, file = file.path(outfolder, 'C_Q.csv'))
    # write.csv(age_matr, file = file.path(outfolder, 'age_matr.csv'))
    #-----------
    
  }
}

#--------------------------------------------------------------------------
# notation details:
# T: age
# t: time
# S: total system storage
# pS: storage age distribution
# pQ: discharge age distribution
# Ps: cumulative age distribution of the system storage
# S_T=S*P_S: rank storage
# Omega: cumulative StorAge Selection function

# all the 'diff' functions return the derivative (df/dx) multiplied by some increment:
# diff(T) represents dT
# diff(Ps) represents (dPs/dT)*dT=pS*dT
# diff(S_T) represents (dS_T/dT)*dT=S*pS*dT
# diff(Omega) represents omega(S_T)*dS_T or equally omega(Ps)*dPs or pQ(T)*dT

# so if one wants the 'classic' variables, some conversion is needed:
# Ps = S_T/S
# omega(S_T) = diff(Omega(S_T))/diff(S_T)
# omega(Ps) = diff(Omega(Ps))/diff(Ps)
# pS(T) = diff(Ps)/diff(T)
# pQ(T) = diff(OmegaQ)/diff(T)
#--------------------------------------------------------------------------
