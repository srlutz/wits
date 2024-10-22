if(manual_input=="N")
{
# choice 1: temporal resolution of the data? !!! Inactivated !!!
#sas_res<-dlgInput("Give the temporal resolution of your data in hours (e.g., daily: 24, weekly: 168, or same as input: 1)", Sys.info()["user"])$res
#while (sas_res<=0) {sas_res<-dlgInput("The answer must be positive and not 0", Sys.info()["user"])$res}
#sas_res <- as.numeric(sas_res)
sas_res <- 1

# choice 2: weighting of the input function
weight_input<-dlgInput("Which weighting should be applied for the input (1<-no weighting, 2<-calculated from the shift in isotopic value between precipitation and discharge, [0;1]<-user defined (enter a value between 0 and 1))", Sys.info()["user"])$res
while (weight_input<0 |  weight_input>1 & weight_input!=2) {weight_input<-dlgInput("The answer can only be between 0 and 1, or equal to 2", Sys.info()["user"])$res}
weight_input <- as.numeric(weight_input)
if(weight_input==1) weight_months<-c(1,2,3,4,5,6,7,8,9,10,11,12) # if the weighting option is not selected, all months are considered to contribute equally to the isotope input
if(weight_input!=1) weight_months<-dlgInput("Which months should be weighted (1<-January, 2<-February, etc..)? Enter each month number, separated by a comma. Normally 4,5,6,7,8,9", Sys.info()["user"])$res
if(weight_input!=1) weight_months <- as.numeric(unlist(strsplit(weight_months, ",")))

# choice 3: SAS function shape
sas_fun<-dlgInput("Which SAS function should be used? Enter 1, 2, or 3! (1: fixed power-law, 2: time-variant power-law, 3: beta shape)", Sys.info()["user"])$res
while (sas_fun!="1" & sas_fun!="2" & sas_fun!="3") {sas_fun<-dlgInput("The answer must be 1, 2, or 3!", Sys.info()["user"])$res}
sas_fun <- as.numeric(sas_fun)

if (sas_fun==1) {
  kQ_range<-dlgInput("Give min und max values for k_Q (discharge flux) to search the optimal solution, separated by comma", Sys.info()["user"])$res
  kQ_range <- as.numeric(unlist(strsplit(kQ_range, ",")))
  kET_range<-dlgInput("Give min und max values for k_ET (ET flux) to search the optimal solution, separated by comma", Sys.info()["user"])$res
  kET_range <- as.numeric(unlist(strsplit(kET_range, ",")))
  stor_range<-dlgInput("Give min und max values for the intial storage in [mm] to search the optimal solution, separated by comma", Sys.info()["user"])$res
  stor_range <- as.numeric(unlist(strsplit(stor_range, ",")))
}

if (sas_fun==2) {
  kminQ_range<-dlgInput("Give min und max values for kmin_Q to search the optimal solution, separated by comma", Sys.info()["user"])$res
  kminQ_range <- as.numeric(unlist(strsplit(kminQ_range, ",")))
  kmaxQ_range<-dlgInput("Give min und max values for kmax_Q to search the optimal solution, separated by comma", Sys.info()["user"])$res
  kmaxQ_range <- as.numeric(unlist(strsplit(kmaxQ_range, ",")))
  betaET_range<-dlgInput("Give min und max values for beta_ET to search the optimal solution, separated by comma", Sys.info()["user"])$res
  betaET_range <- as.numeric(unlist(strsplit(betaET_range, ",")))
  stor_range<-dlgInput("Give min und max values for the intial storage in [mm] to search the optimal solution, separated by comma", Sys.info()["user"])$res
  stor_range <- as.numeric(unlist(strsplit(stor_range, ",")))
}

if (sas_fun==3) {
  alpha_range<-dlgInput("Give min und max values for alpha to search the optimal solution, separated by comma", Sys.info()["user"])$res
  alpha_range <- as.numeric(unlist(strsplit(alpha_range, ",")))
  beta_range<-dlgInput("Give min und max values for beta to search the optimal solution, separated by comma", Sys.info()["user"])$res
  beta_range <- as.numeric(unlist(strsplit(beta_range, ",")))
  betaET_range<-dlgInput("Give min und max values for beta_ET to search the optimal solution, separated by comma", Sys.info()["user"])$res
  betaET_range <- as.numeric(unlist(strsplit(betaET_range, ",")))
  stor_range<-dlgInput("Give min und max values for the intial storage in [mm] to search the optimal solution, separated by comma", Sys.info()["user"])$res
  stor_range <- as.numeric(unlist(strsplit(stor_range, ",")))
}

# choice 4: initial storage tracer value
sas_ini<-dlgInput("Give the initial tracer value of the catchment storage (in permille or TU. Must be a number!)", Sys.info()["user"])$res
sas_ini <- as.numeric(sas_ini)*(-1) #for plotting reasons, stable isotope values should be positive

# choice 5: which TTD to display?
date_TTD<-dlgInput("Give ONE date for which transit times should be plotted (yyyy-mm-dd)", Sys.info()["user"])$res
date_TTD <- as.character(unlist(strsplit(date_TTD, ",")))

# choice 6: steady state or transient
flow_dyn<-dlgInput("Should calculations be made for steady state or transient conditions (SS<- steady state, TRAN<- transient)?", Sys.info()["user"])$res
while (flow_dyn!="SS" &  flow_dyn!="TRAN") {calib<-dlgInput("The answer can only be one of the following: SS<- steady state, TRAN<- transient", Sys.info()["user"])$res}

# additional default information

#weight_input<-1 # weighting of the input function. Not implemented for the SAS calculations (only for LPM), but necessary as condition in the module "data_import.R"


flag <- 4 # minimum number of fitting parameters
if(tracer=="H3+ST") flag <- flag+1 # adds one parameter if both tracers are used for calibration
if(sas_fun==2 | sas_fun==2)  flag <- flag+1 # adds one parameter if the model requires a second one
nit<-10^flag # number of iterations for the GLUE algorithm. DO NOT set it too low, as the high number of fitting parameters means that the calibration space is very large, and will not be properly explored with too few pseudo-random draws. The default chosen here is 10^number of fitting parameters (3 to 5 depending on tracer combination and model choice, plus one for safety)

}

if(manual_input=="Y")
{
# choice 1: temporal resolution of the data?
sas_res<-1 # Give the temporal resolution of your data in hours (e.g., daily: 24, weekly: 168, or same as input: 1)", Sys.info()["user"])$res

# choice 2: weighting of the input function
weight_input<-1 # 1<-no weighting, 2<-calculated from the shift in isotopic value between precipitation and discharge, [0;1]<-user defined (enter a value between 0 and 1))
weight_months<-c(4,5,6,7,8,9) # Which months should be weighted (1<-January, 2<-February, etc..)? Enter each month number, separated by a comma. In the Northern hemisphere, normally 4,5,6,7,8,9". !!!! If weight_input==1, weight_months <- c(1,2,3,4,5,6,7,8,9,10,11,12) !!!!

# choice 3: SAS function shape
sas_fun<-1 # Which SAS function should be used? Enter 1, 2, or 3 (1: fixed power-law, 2: time-variant power-law, 3: beta shape)", Sys.info()["user"])$res

    # additional SAS parameters
if (sas_fun==1) {
kQ_range<-c(0.1,1) # Give min und max values for k_Q (discharge flux) to search the optimal solution, separated by comma
kET_range<-c(0.1,1) # Give min und max values for k_ET (ET flux) to search the optimal solution, separated by comma
stor_range<-c(1,200) # Give min und max values for the intial storage in [mm] to search the optimal solution, separated by comma
}
if (sas_fun==2) {
kminQ_range<-c(0.1,1) # Give min und max values for kmin_Q to search the optimal solution, separated by comma
kmaxQ_range<-c(0.1,1) # Give min und max values for kmax_Q to search the optimal solution, separated by comma
betaET_range<-c(0.1,1) # Give min und max values for beta_ET to search the optimal solution, separated by comma
stor_range<-c(1,200)  # Give min und max values for the intial storage in [mm] to search the optimal solution, separated by comma
}
if (sas_fun==3) {
alpha_range<-c(0.1,1) # Give min und max values for alpha to search the optimal solution, separated by comma
beta_range<-c(0.1,1)  # Give min und max values for beta to search the optimal solution, separated by comma
beta_range <- as.numeric(unlist(strsplit(beta_range, ",")))
betaET_range<-c(0.1,1) # Give min und max values for beta_ET to search the optimal solution, separated by comma
stor_range<-c(1,200)  # Give min und max values for the intial storage in [mm] to search the optimal solution, separated by comma
}
# choice 4: initial storage tracer value
sas_ini<--50 # Give the initial tracer value of the catchment storage (in permille or TU. Must be a number!)
sas_ini <- as.numeric(sas_ini)*(-1) #for plotting reasons, stable isotope values should be positive

# choice 5: which TTDs to display?
date_TTD<-c() # Give ONE date for which transit times should be plotted (yyyy-mm-dd)
date_TTD <- as.character(unlist(strsplit(date_TTD, ",")))

# choice 6: steady state or transient
#flow_dyn<-1 # Dummy: Currently SAS runs in SS, but later a choice between SS and trans will be made. Enter 1 without consequences
flow_dyn<-"SS" # if steady state is chosen, the user does not have to provide time series for rainfall, discharge, weights and evapotranspiration, as these will be created automatically with constant values.

# additional default information

weight_input<-1 # weighting of the input function. Not implemented for the SAS calculations (only for LPM), but necessary as condition in the module "data_import.R"

flag <- 4 # minimum number of fitting parameters
if(tracer=="H3+ST") flag <- flag+1 # adds one parameter if both tracers are used for calibration
if(sas_fun==2 | sas_fun==2)  flag <- flag+1 # adds one parameter if the model requires a second one
nit<-10^flag # number of iterations for the GLUE algorithm. DO NOT set it too low, as the high number of fitting parameters means that the calibration space is very large, and will not be properly explored with too few pseudo-random draws. The default chosen here is 10^number of fitting parameters (3 to 5 depending on tracer combination and model choice, plus one for safety)


}


# vertical offset for the output time series and number of iteration for the calibration (automatic-not user defined)

offset_range <- c(-5,5)
if(tracer=="H3+ST" | tracer=="ST") {if(stable_iso == "O18") offset_range<-c(-1,1)} # parameter controlling the offset of the modelled output to match the measured output !!!! This parameter is a fitting parameter that may be necessary in the case of fitting the seasonal signal of stable isotopes, but care should be taken when setting it to a value different from 0. Normally, the infiltration coefficient should be sufficient to balance out the difference in mean value between input and output, especially in the case of tritium.




