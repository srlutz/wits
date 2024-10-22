if(manual_input=="N")
{
# choice 1: steady state or transient calculations?
flow_dyn<-dlgInput("Should calculations be made for steady state or transient conditions (SS<- steady state, TRAN<- transient)?", Sys.info()["user"])$res
while (flow_dyn!="SS" &  flow_dyn!="TRAN") {flow_dyn<-dlgInput("The answer can only be one of the following: SS<- steady state, TRAN<- transient", Sys.info()["user"])$res}
if(flow_dyn=="TRAN") {td<-dlgInput("Give the values for the td parameter (in years) used for transient conditions and considered during model calibration. This parameter is not calibrated and must be estimated beforehand!", Sys.info()["user"])$res
td <- as.numeric(unlist(strsplit(td, ",")))
if(td[1]<=0) {td<-dlgInput("The td parameter cannot be equal to 0. Please enter its value again.", Sys.info()["user"])$res; td <- as.numeric(unlist(strsplit(td, ",")))}
td<-td*time_unit} # changing the time unit of the td parameter from years to the unit chosen by the user

# choice 2: which transfer function should be used (lumped parameter models)?
if(flow_dyn=="SS") lpm_model<-dlgInput("Which lumped parameter model should be used (EM<-exponential model, GM<-gamma model, DM<-dispersion model, DEM <- double exponential) ?", Sys.info()["user"])$res
if(flow_dyn=="TRAN") lpm_model<-dlgInput("Which lumped parameter model should be used (EM<-exponential model, DM<-dispersion model)?", Sys.info()["user"])$res
while (lpm_model!="EM" &  lpm_model!="GM" & lpm_model!="DM" & lpm_model!="DEM") {lpm_model<-dlgInput("The answer can only be one of the following: EM<-exponential model, GM<-gamma model, DM<-dispersion model", Sys.info()["user"])$res}

if(lpm_model=="DEM") {t_dem<-dlgInput("Give the values for the tdem parameter (in years) corresponding to the mean transit time of the quickflow reservoir of the double exponential model. This fitting parameter is not calibrated !", Sys.info()["user"])$res
t_dem <- as.numeric(unlist(strsplit(t_dem, ",")))
if(t_dem[1]<=0) {t_dem<-dlgInput("The t_dem parameter cannot be equal to 0. Please enter its value again.", Sys.info()["user"])$res; t_dem <- as.numeric(unlist(strsplit(t_dem, ",")))}
t_dem<-t_dem*time_unit} # changing the time unit of the t_dem parameter from years to the unit chosen by the user

# choice 3: weighting of the input function
weight_input<-dlgInput("Which weighting should be applied for the input (1<-no weighting, 2<-calculated from the shift in isotopic value between precipitation and discharge, [0;1]<-user defined (enter a value between 0 and 1))", Sys.info()["user"])$res
while (weight_input<0 |  weight_input>1 & weight_input!=2) {weight_input<-dlgInput("The answer can only be between 0 and 1, or equal to 2", Sys.info()["user"])$res}
weight_input <- as.numeric(weight_input)
if(weight_input==1) weight_months<-c(1,2,3,4,5,6,7,8,9,10,11,12) # if the weighting option is not selected, all months are considered to contribute equally to the isotope input
if(weight_input!=1) weight_months<-dlgInput("Which months should be weighted (1<-January, 2<-February, etc..)? Enter each month number, separated by a comma. Normally 4,5,6,7,8,9", Sys.info()["user"])$res
if(weight_input!=1) weight_months <- as.numeric(unlist(strsplit(weight_months, ",")))
if (weight_input==2) {stable_iso<-dlgInput("Which stable isotope should be used for weighting (O18<- oxygen-18, H2<- deuterium)?", Sys.info()["user"])$res;while (stable_iso!="O18" &  stable_iso!="H2") {stable_iso<-dlgInput("The answer can only be one of the following: O18<- oxygen-18, H2<- deuterium", Sys.info()["user"])$res}}

# choice 4: search interval for the mean transit time
mtt_range<-dlgInput("Give the mean transit time range in years. Enter two values, separated by a comma, for instance 1,10 if the mean transit time is expected to be between 1 and 10 years", Sys.info()["user"])$res
mtt_range <- as.numeric(unlist(strsplit(mtt_range, ",")))

# choice 5: search interval for the second fitting parameter of the transfer function (if any)
if(lpm_model=="GM" | lpm_model=="DM" | lpm_model=="DEM") par2_range<-dlgInput("Give the minimum and maximum values for the second fitting parameter (gamma model:alpha, dispersion model: PD=1/Pe, double exp: relative weight of the shorter transit times) considered during model calibration. Enter two values, separated by a comma, for instance 0.1,1", Sys.info()["user"])$res
if(lpm_model=="GM" | lpm_model=="DM" | lpm_model=="DEM") par2_range <- as.numeric(unlist(strsplit(par2_range, ",")))
if(lpm_model=="GM" | lpm_model=="DM" | lpm_model=="DEM") {while(par2_range[1]<=0) {par2_range<-dlgInput("The second parameter cannot be equal to 0. Please enter an interval again.", Sys.info()["user"])$res; par2_range <- as.numeric(unlist(strsplit(par2_range, ",")))}}
if(lpm_model=="DEM") {while(par2_range[2]>=1) {par2_range<-dlgInput("The second parameter cannot be equal to 1. Please enter an interval again.", Sys.info()["user"])$res; par2_range <- as.numeric(unlist(strsplit(par2_range, ",")))}}

# choice 6: search interval for the piston flow component
par4_range<-dlgInput("Give the minimum and maximum values for the piston-flow shift in years considered during model calibration. Enter two values, separated by a comma, for instance 0.1,1. If unnecessary, set the range to 0,0. A value of 0 means no piston-flow.", Sys.info()["user"])$res
par4_range <- as.numeric(unlist(strsplit(par4_range, ",")))

# choice 7: which TTD to display?
if(flow_dyn=="TRAN") {
date_TTD<-dlgInput("Give ONE date for which the transit time distribution should be plotted (yyyy-mm-dd)", Sys.info()["user"])$res
date_TTD <- as.character(unlist(strsplit(date_TTD, ",")))
}

# vertical offset for the output time series and number of iteration for the calibration (automatic-not user defined)

offset_range <- c(-5,5)
if(tracer=="H3+ST" | tracer=="ST") {if(stable_iso == "O18") offset_range<-c(-1,1)} # parameter controlling the offset of the modelled output to match the measured output !!!! This parameter is a fitting parameter that may be necessary in the case of fitting the seasonal signal of stable isotopes, but care should be taken when setting it to a value different from 0. Normally, the infiltration coefficient should be sufficient to balance out the difference in mean value between input and output, especially in the case of tritium.

# additional choice not accessible to user
flag <- 4 # minimum number of fitting parameters
if(tracer=="H3+ST") flag <- flag+1 # adds one parameter if both tracers are used for calibration
if(lpm_model=="GM" | lpm_model=="DM"  | lpm_model=="DEM")  flag <- flag+1 # adds one parameter if the model requires a second one
nit<-10^flag # number of iterations for the GLUE algorithm. DO NOT set it too low, as the high number of fitting parameters means that the calibration space is very large, and will not be properly explored with too few pseudo-random draws. The default chosen here is 10^number of fitting parameters (3 to 5 depending on tracer combination and model choice, plus one for safety)


}

if(manual_input=="Y")
{
# choice 1: which transfer function should be used (lumped parameter models)?
lpm_model<-"EM" # EM<-exponential model, GM<-gamma model, DM<-dispersion model, DEM <- double exponential model

t_dem<- 1/12 # Only necessary for the double exponential model. Give the values for the t_dem parameter (in years) corresponding to the mean transit time of the quickflow reservoir of the double exponential model. This fitting parameter is not calibrated !
t_dem<-t_dem*time_unit # changing the time unit of the t_dem parameter from years to the unit chosen by the user

# choice 2: weighting of the input function
weight_input<-1 # 1<-no weighting, 2<-calculated from the shift in isotopic value between precipitation and discharge, [0;1]<-user defined (enter a value between 0 and 1))
weight_months<-c(4,5,6,7,8,9) # Which months should be weighted (1<-January, 2<-February, etc..)? Enter each month number, separated by a comma. In the Northern hemisphere, normally 4,5,6,7,8,9". !!!! If weight_input==1, weight_months <- c(1,2,3,4,5,6,7,8,9,10,11,12) !!!!
stable_iso<-"O18" # Which stable isotope should be used for weighting (O18<- oxygen-18, H2<- deuterium)?

# choice 3: steady state or transient calculations?
flow_dyn<-"SS" # SS<- steady state, TRAN<- transient

td<-0.1 # Give the values for the td parameter (in years) used for transient conditions and considered during model calibration. This parameter is not calibrated and must be estimated beforehand!
td<-td*time_unit # changing the time unit of the td parameter from years to the unit chosen by the user

# choice 4: search interval for the mean transit time
mtt_range<-c(1,10) # Give the minimum and maximum values for the mean transit time in years.

# choice 5: search interval for the second fitting parameter of the transfer function (if any)
par2_range<-c(0,2) # Give the minimum and maximum values for the second fitting parameter (gamma model:alpha, dispersion model: PD=1/Pe, double exponential: relative weight of the shorter transit times ) considered during model calibration.

# choice 6: search interval for the piston flow component
par4_range<-c(0,1) # Give the minimum and maximum values for the piston-flow shift in years considered during model calibration. If unnecessary, set the range to 0,0. A value of 0 means no piston-flow

# choice 7: which TTD to display?
date_TTD<-c("1973-01-01") # Give ONE date for which transit times should be plotted (yyyy-mm-dd)
#date_TTD <- as.character(unlist(strsplit(date_TTD, ",")))

# vertical offset for the output time series and number of iteration for the calibration (automatic-not user defined)

offset_range <- c(-5,5)
if(tracer=="H3+ST" | tracer=="ST") {if(stable_iso == "O18") offset_range<-c(-1,1)} # parameter controlling the offset of the modelled output to match the measured output !!!! This parameter is a fitting parameter that may be necessary in the case of fitting the seasonal signal of stable isotopes, but care should be taken when setting it to a value different from 0. Normally, the infiltration coefficient should be sufficient to balance out the difference in mean value between input and output, especially in the case of tritium.

# additional choice not accessible to user
flag <- 4 # minimum number of fitting parameters
if(tracer=="H3+ST") flag <- flag+1 # adds one parameter if both tracers are used for calibration
if(lpm_model=="GM" | lpm_model=="DM"  | lpm_model=="DEM")  flag <- flag+1 # adds one parameter if the model requires a second one
nit<-10^flag # number of iterations for the GLUE algorithm. DO NOT set it too low, as the high number of fitting parameters means that the calibration space is very large, and will not be properly explored with too few pseudo-random draws. The default chosen here is 10^number of fitting parameters (3 to 5 depending on tracer combination and model choice, plus one for safety)


}


