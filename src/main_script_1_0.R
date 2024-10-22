rm(list = ls()) #cleans global environment

#######################################################
########## script with which                          #
########## to fit lumped-parameter and SAS models     #
########## to tritium and stable isotope measurements #
#######################################################

##### version 0.7

##### Authors: Stefanie Lutz (s.r.lutz@uu.nl), Michael Stockinger (michael_stockinger@boku.ac.at), Sascha Mueller (sascha.muller@biol.lu.se), Julien Farlin (julien.farlin@eau.etat.lu)


##################################
# workspace definition and setup #
##################################

#### IMPORTANT NOTE ####
# This is the only section where a manual input from the user is necessary.
# 1. The user HAS to enter in "folder<-...." the correct path on his computer to the folder containing all the data necessary for computations. Make also sure that either "/" or "\" are used for the subdirectories listed in lines 35 to 40, depending on operating system.
# 2. ADDITIONALLY, the user can override the popup windows used to enter all modelling choices, in order to enter these directly in the appropriate text files instead. In that case only will the user need to modify other files before running the main script. Otherwise, once the root folder has been indicated, the user saves the current script, and then runs it in R.

# The folder setup of the example must be kept as is, simply changing the local host name to the main folder 

folder<-c("/home/jfa/Documents/science/AGE/cost_watson/WG3/scripts/development/github_1_0/") # put here your local host name

data_input_dir<-paste(folder, "data", sep = "")
routines_dir<-paste(folder, "src", sep = "")
results_dir<-paste(folder, "results", sep ="")
SAS_dir<-paste(folder, "src/SAS/", sep = "")
functions_dir<-paste(folder, "src/SAS/functions/", sep = "")
models_dir<-paste(folder, "src/SAS/models/", sep = "")


manual_input="N" # FOR EXPERIENCED USERS ONLY!!! This switch decides whether the user wishes to override the popup windows and to enter all modelling parameters DIRECTLY in the appropriate text files. By default set to "N" for "no". If manual input is desired, set it to "Y" for yes instead. This option is for experienced users as it requires modifying and saving input files BEFORE running this script.

####################################
# loading necessary libraries      #
####################################

# Installs and loads the required libraries
setwd(routines_dir)
source("package_installation.R")

###########################
# User defined parameters #
###########################

# !!!! If the user wishes to enter all choices directly in the text files, and has set "manual_input" to "Y", then "user_choices_common.R" and either "user_choices_LPM.R" or "user_choices_SAS.R" have to be opened, the choices entered manually and saved, before running the present script.

source("user_choices_common.R") # user defined options common to both approaches: 1. lumped parameter models or storage selection functions 2. How many isotopes and which stable isotope 3. Which algorithm used for calibration 4. Which objective function used for calibration

if(method=="LPM") source("user_choices_LPM.R")  # choices specific to the lumped parameter models
if(method=="SAS") source("user_choices_SAS.R")  # choices specific to the storage selection functions

result_summary<- as.data.frame(matrix(NA,ncol=10,nrow=1)) #run_nr,model_type,MTT,err
if(method=="SAS") result_summary<- as.data.frame(matrix(NA,ncol=12,nrow=1)) #run_nr,model_type,MTT,err

modif<-0

for(k in 1:20) { # up to 20 runs can be done, changing one element each time (for instance the LPM model used, the objective function, etc..)

    #########################
    # loading the data      #
    #########################

    source("data_import.R")

    ############################
    # Preprocessing the raw data
    ############################
      
    # interpolate missing values of the input functions

    if(time_unit==1 | time_unit==12 | time_unit==55) source("input_interpolation_version0_4.R") # gaps in the input are interpolated, unless the user has chosen to work directly with the raw data

    # Calculation of the summer infiltration coefficient used as weighting factor for the input functions (from Grabczak et al., Catena 11, 1984, 105-114) and weighting of the input functions. This routine creates two weighted input time series, one for tritium, the other for the stable isotope. This can only be done if stable isotope measurements at the outlet are available. Otherwise, user defined values are used to weight the input functions. NOTE: no weighting is applied before SAS calculation or if the raw values are used

    if(time_step!="R") source("infiltration_coefficient_version0_2.R")

    # Calculation of monthly averages for the isotopes output (and sum of discharge if the variable flow formulation has been chosen). Only done if selected by the user. Otherwise, the calculations are performed at the time step of the input files.
    if(time_step=="MM") source("monthly_average_version0_1.R") #monthly output averages are calculated if the user has chosen this option
    if(time_step=="AM") source("annual_average_version0_1.R") #yearly output averages are calculated if the user has chosen this option

    # loads the different transfer functions.
    if (method == "LPM") source("MTT_functions_version0_1c.R") #only for LPM relevant

    if(method == "LPM" & flow_dyn=="TRAN") source("MTT_functions_version0_1d.R") # if the variable flow formulation has been chosen, the corresponding transfer functions are selected and replace those used for steady state calculations. Only for LPM relevant

    ######################################################################
    ### Calibration
    ######################################################################

    # defines the input and output time series used for calibration.

    source("calibration_data.R")

    # creates the directories where the calibration results are stored.
    source("calibration_setup.R") 

    ######################################################################
    ### Monte Carlo simulations
    ######################################################################

    #### First step: input-output modeling for different parameter combinations  ####

    # sets up the parameter combinations and the input and output data

    source("monte-carlo_setup.R")

    # depending on the method chosen, the output will be calculated either using the classical lumped parameter models or the storage selection functions.

    if(method=="LPM") source("monte-carlo_LPM.R")
    if(method=="SAS") source("monte-carlo_SAS.R")


    #### Second step: finding and saving the best fits  ####

    #setwd(routines_dir)
    if(method=="LPM") source("parameter_estimation_LPM.R")
    if(method=="SAS") source("parameter_estimation_SAS.R")

    ###################################################
    # Post-processing
    ###################################################

    #### summary of all input parameters for the run #####

    if(method=="LPM") source("parameter_run_summary_lpm.R")
    if(method=="SAS") source("parameter_run_summary_sas.R")

    #### graphic labels and boundaries #####

    source("graphics_labels_boundaries.R")

    #### creating the graphical outputs ######

    # input graphs

    source("graphics_inputs.R")

    # output graphs

    source("graphics_outputs.R")

    # storing the important parameters for all runs consecutively
 
    if(method=="LPM") {  
    names(result_summary)<-c("Run","Model","MTT range","alpha/PD/fraction-dem range","MTT","alpha/PD/fraction-dem","weight","Error","PF","Change")
    result_summary[as.numeric(k),]<-c(as.numeric(k),lpm_model,paste(p2[1],"-",p2[2]),paste(p3[1],"-",p3[2]),round(res3$mtt,4),round(res3$`alpha/PD`,4),alpha,round(res3$error,4),round(res3$`piston-flow`,4),modif)
        } 

    if(method=="SAS") {  
    names(result_summary)<-c("Run","Model","storage range","kQ/kQmin/alpha range","na/kQmax/beta range","kET/betaET/betaET range","storage","kQ/kQmin/alpha","na/kQmax/beta","kET/betaET/betaET","Error","Change")
    if(sas_fun==1) result_summary[as.numeric(k),]<-c(as.numeric(k),sas_fun,paste(stor_range[1],"-",stor_range[2]),paste(kQ_range[1],"-",kQ_range[2]),"na",paste(kET_range[1],"-",kET_range[2]),round(res3$storage,4),round(res3$kQ,4),"na",round(res3$kET,4),round(res3$error,4),modif)

    if(sas_fun==2) result_summary[as.numeric(k),]<-c(as.numeric(k),sas_fun,paste(stor_range[1],"-",stor_range[2]),paste(kminQ_range[1],"-",kminQ_range[2]),paste(kmaxQ_range[1],"-",kmaxQ_range[2]),paste(betaET_range[1],"-",betaET_range[2]),round(res3$storage,4),round(res3$kQmin,4),round(res3$kQmax,4),round(res3$betaET,4),round(res3$error,4),modif)

    if(sas_fun==3) result_summary[as.numeric(k),]<-c(as.numeric(k),sas_fun,paste(stor_range[1],"-",stor_range[2]),paste(alpha_range[1],"-",alpha_range[2]),paste(beta_range[1],"-",beta_range[2]),paste(betaET_range[1],"-",betaET_range[2]),round(res3$storage,4),round(res3$alpha,4),round(res3$beta,4),round(res3$betaET,4),round(res3$error,4),modif)
        }  
 
    # running the calculations changing one input parameter

    if(k < 20) {
    setwd(routines_dir)
    if(method=="LPM") source("model_rerun_lpm.R")
    if(method=="SAS") source("model_rerun_sas.R")  }


    }

write.csv(result_summary,paste(results_dir,'/result_summary.csv',sep = ''))
