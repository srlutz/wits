
#--------------------------------------------------------------------------
# 1-GENERAL MODEL SETUP
#--------------------------------------------------------------------------

setwd(SAS_dir)

#--------------------------------------------------------------------------
# 2-GENERAL SIMULATION SETTINGS
#--------------------------------------------------------------------------

# Set the case study and model
ModelName <- 'SAS_EFs'

# Set aggregation timestep and storage threshold
#dt_aggr <- sas_res  ### to be deleted, is moverd from here to the main_script to the calibration_data 
f_thresh <- 1

# Set additional options
create_spinup <- FALSE
save_output <- TRUE
load_output <- TRUE

#--------------------------------------------------------------------------
# 3-PARAMETERS
#--------------------------------------------------------------------------

p1<- output_MC[ix,2] # best fit parameter values
p2<- output_MC[ix,3]
p3<- output_MC[ix,4]
p4<- output_MC[ix,5]
decay <- 0 # switch for the radioactive decay. 0=no decay (stable isotope), 1=tritium decay

  ############################################

data.backup <- data # saves the data object before changing the isotope input in each run by a variable offset.

example <- sas_fun

    data <- data.backup
    data$C_J <- data$C_J*0 # resets the isotope input vector to 0, except for the date for which the transit time distribution should be saved.
    data$C_J[data$index_datesel] <- 1 # dirac impulse scaled to 1 on the injection date chosen by the user.

    # SASQ with fixed power-law shape
    if (example == 1) {
      # SASQ with fixed power-law shape
      data$SASQName <- 'fSAS_pl'
      data$SASETName <- 'fSAS_pl'
      SASQparamnames <- c('k_Q') #grundwasserspeicher saturated zone
      Pars <- p1  #parameter zu oben k_Q, übernehme von eingangsdaten mean(kQ_range) (0.7 was predefined)
      SASETparamnames <- c('k_ET') #ET fixed? 
      Pars <- c(Pars, p2) #take value from above mean(kET_range) (0.7 was predefined)
      otherparamnames <- c('S0') #anfangsspeicher
      Pars <- c(Pars, p3) #take value from initial storage
      data$C_S0 <- 0 #assign the concentration of the initial storage
    }


     # EXAMPLE 2: SASQ with time-variant power-law shape
     if (example == 2) {
       # select the SAS
       data$SASQName <- 'fSAS_pltv' # time-variant power-law SAS
       data$SASETName <- 'fSAS_pl'  # power-law SAS
       
       # parameter names and values
       Pars <- c(0,0,0,0)
       SASQparamnames <- c('kmin_Q', 'kmax_Q')
       Pars[1:2] <- c(p1, p2)  # [-] #here should be the min and max of discharge automatically be written in?
       SASETparamnames <- c('beta_ET')
       Pars[3] <- p3  # [-]
       otherparamnames <- c('S0')
       Pars[4] <- p4  # [mm]
       data$C_S0 <- 0 #assign the concentration of the initial storage
     }
     
     # EXAMPLE 3: SASQ with Beta shape
     if (example == 3) {
       # select the SAS
       data$SASQName <- 'fSAS_beta' # beta SAS
       data$SASETName <- 'fSAS_pl'  # power-law SAS
       
       # parameter names and values
       Pars <- c(0,0,0,0)
       SASQparamnames <- c('alpha', 'beta')
       Pars[1:2] <- c(p1, p2)  # [-]
       SASETparamnames <- c('beta_ET')
       Pars[3] <- p3  # [-]
       otherparamnames <- c('S0')
       Pars[4] <- p4  # [mm]
       data$C_S0 <- 0 #assign the concentration of the initial storage
     }

    # Define model parameters
    data$SASQl <- length(SASQparamnames)
    data$SASETl <- length(SASETparamnames)
    data$paramnames <- c(SASQparamnames, SASETparamnames, otherparamnames)

    #--------------------------------------------------------------------------
    # 4-SELECT THE AGE DISTRIBUTIONS TO EXTRACT
    #--------------------------------------------------------------------------

    # Define selected dates for age distribution extraction
    datesel <- as.numeric(as.Date(date_TTD,"%Y-%m-%d"))

    # Find the closest available dates in the dataset
    data$index_datesel <- sapply(datesel, function(date) {
      min(which(abs(data$dates - date) == min(abs(data$dates - date))))
    })

    #--------------------------------------------------------------------------
    # 5-INITIAL CONDITIONS
    #--------------------------------------------------------------------------

    source(paste(functions_dir,"fgenerate_spinup.R",sep=""))

    # Set spinup settings
    period_rep <- c(1, 365 * 4)
    n_rep <- 1

    if (create_spinup) {
      data <- fgenerate_spinup(data, period_rep, n_rep)
    } else {
      data$ini_shift <- 0
    }

    #--------------------------------------------------------------------------
    # 6-RUN THE MODEL
    #--------------------------------------------------------------------------

    source(paste(models_dir,"SAS_EFs.R",sep=""))

    # Store additional entries into the data structure
    data$f_thresh <- f_thresh
    data$save_output <- save_output
    data$outputchoice <- 'C_Qsampl'

    # Run the model
    #cat('calculating model output...\n')
    out <- do.call(ModelName, c(list(Pars, data)))
    min <- data$index_datesel+1
    max <- length(C_Q)
    TTD <- C_Q[min:max]
    TTD <- data.frame(timestep_x=seq(1,length(TTD)),TTD = TTD)

setwd(routines_dir)

