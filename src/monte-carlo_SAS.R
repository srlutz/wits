
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

obs <- data$measC_Q

# creates a matrix with all parameter combinations in a given range
if(p1[1]==p1[2])  {dummy1=seq(p1[1],p1[2],length.out=1)} else {dummy1=seq(p1[1],p1[2],length.out=discretisation)} # if the first parameter should not be fitted, it is set to the one value given by the user.
if(p2[1]==p2[2])  {dummy2=seq(p2[1],p2[2],length.out=1)} else {dummy2=seq(p2[1],p2[2],length.out=discretisation)} # if the second parameter should not be fitted, it is set to the one value given by the user.
if(p3[1]==0 & p3[2]==0)  {dummy3=seq(p3[1],p3[2],length.out=1)} else {dummy3=seq(p3[1],p3[2],length.out=discretisation)} # if the third parameter should not be fitted, it is set to the one value given by the user.
if(sas_fun==1) {dummy4=seq(0,length.out=1)} else {dummy4=seq(p4[1],p4[2],length.out=discretisation)} # if the fourth fitting parameter is not needed (i.e. for SAS model 1), it is set to "0" and only one value is generated in the parameter combination object
if(p5[1]==p5[2])  {dummy5=seq(p5[1],p5[2],length.out=1)} else {dummy5=seq(p5[1],p5[2],length.out=discretisation)} # if the offset parameter should not be fitted, it is set to the one value given by the user.
params<-do.call(expand.grid,list(dummy1,dummy2,dummy3,dummy4,dummy5)) # creates all parameter combinations
names(params)<- c("p1","p2","p3","p4","p5")
  
#
out_df <- data.frame(err=double(),param1=double(),param2=double(),param3=double(),param4=double()) # object storing the model parameters of each run
outiso_df <- data.frame(matrix(ncol = length(data$C_J), nrow = 0)) # object storing the modelling results of each run
outresid_df <- data.frame(matrix(ncol = length(data$measC_Q), nrow = 0)) # object storing the residuals of each run (observed minus predicted)
out_ttd <- data.frame(matrix(ncol = length(data$C_J), nrow = 0)) # object storing the modelling results of each run
EM_dist <- 0*vector(mode="numeric",length=length(data$C_J)) # object storing the transit time distribution

p1<- params$p1 # parameter vectors for the monte carlo loop
p2<- params$p2
p3<- params$p3
p4<- params$p4
p5<- params$p5

  ############################################

data.backup <- data # saves the data object before changing the isotope input in each run by a variable offset.

example <- sas_fun
  
  # start the loop of the iterations
  for(ii in 1:dim(params)[1]){

    print(paste(100*(ii/dim(params)[1]),"% - calibration"))

    data <- data.backup
    data$C_J <- data$C_J++p5[ii] # adds an offset to the isotope input

    # SASQ with fixed power-law shape
    if (example == 1) {
      # SASQ with fixed power-law shape
      data$SASQName <- 'fSAS_pl'
      data$SASETName <- 'fSAS_pl'
      SASQparamnames <- c('k_Q') #grundwasserspeicher saturated zone
      Pars <- p1[ii]  #parameter zu oben k_Q, übernehme von eingangsdaten mean(kQ_range) (0.7 was predefined)
      SASETparamnames <- c('k_ET') #ET fixed? 
      Pars <- c(Pars, p2[ii]) #take value from above mean(kET_range) (0.7 was predefined)
      otherparamnames <- c('S0') #anfangsspeicher
      Pars <- c(Pars, p3[ii]) #take value from initial storage
      data$C_S0 <- sas_ini #assign the concentration of the initial storage
    }


     # EXAMPLE 2: SASQ with time-variant power-law shape
     if (example == 2) {
       # select the SAS
       data$SASQName <- 'fSAS_pltv' # time-variant power-law SAS
       data$SASETName <- 'fSAS_pl'  # power-law SAS
       
       # parameter names and values
       Pars <- c(0,0,0,0)
       SASQparamnames <- c('kmin_Q', 'kmax_Q')
       Pars[1:2] <- c(p1[ii], p2[ii])  # [-] #here should be the min and max of discharge automatically be written in?
       SASETparamnames <- c('beta_ET')
       Pars[3] <- p3[ii]  # [-]
       otherparamnames <- c('S0')
       Pars[4] <- p4[ii]  # [mm]
       data$C_S0 <- sas_ini #assign the concentration of the initial storage
     }
     
     # EXAMPLE 3: SASQ with Beta shape
     if (example == 3) {
       # select the SAS
       data$SASQName <- 'fSAS_beta' # beta SAS
       data$SASETName <- 'fSAS_pl'  # power-law SAS
       
       # parameter names and values
       Pars <- c(0,0,0,0)
       SASQparamnames <- c('alpha', 'beta')
       Pars[1:2] <- c(p1[ii], p2[ii])  # [-]
       SASETparamnames <- c('beta_ET')
       Pars[3] <- p3[ii]  # [-]
       otherparamnames <- c('S0')
       Pars[4] <- p4[ii]  # [mm]
       data$C_S0 <- sas_ini #assign the concentration of the initial storage
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

    output_LPM=C_Q # entire predicted output
    EM_dist[1:length(pQ.out)]=pQ.out # gets transit time distribution, either steady state or for variable flow. In the latter case, one date only is exported, as defined by the user
    #iso_mod <- output_LPM[time.period[1]:time.period[2]] # saves predictions
    iso_mod <- C_Q[data$indexC_Q] # saves predicted output on observation dates
    resid_mod <- iso_mod-obs # saves residuals
    # compute different objective functions
    NS <- max(0 , 1 - var(iso_mod-obs) / var(obs))
    #produces currently warning because of negative sqrt (negative d18O value)
    #CM <- max(0 , 1 - sum( ( sqrt(iso_mod) - sqrt(obs) )^2 ) / 
    # sum( ( sqrt(obs) - sqrt(mean(obs)))^2 ) )
    NSE <- max(0 , 1 - (sum((obs-iso_mod)^2)) / (sum((obs-mean(obs))^2))) # Nash-Sutcliff
    MPE=sqrt(sum((iso_mod-obs)^2))/length(obs) # mean prediction error
    if (obj_fun=="NSE") err=NSE   
    if (obj_fun=="MPE") err=MPE 

    #append output for ii-th simulation
    out_df<-rbind(out_df,data.frame("err"=err,"param1"=p1[ii],"param2"=p2[ii],"param3"=p3[ii],"param4"=p4[ii])) 
    outiso_df<-rbind(outiso_df,output_LPM)
    outresid_df<-rbind(outresid_df,resid_mod)
    out_ttd<-rbind(out_ttd,EM_dist)

}

if(tracer=="ST") outiso_df <- -outiso_df # restores the negative sign for the stable isotopes
#update colnames in simulated data
dates <- paste("D", 1:length(data$C_J), sep="")
colnames(outiso_df)<-dates 
  
write.csv(out_df,file=output,row.names=F)
write.csv(outiso_df,file=output_sim,row.names=F)
write.csv(outresid_df,file=resid_sim,row.names=F)

setwd(routines_dir)

