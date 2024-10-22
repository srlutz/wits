
if(tracer=="H3+ST" | tracer=="ST") {
inputseries1<-stable_input # selects the input time series (stable isotope)
inputdates1<-stable_input_date
if(time_step=="MM" | time_step=="AM") outputseries1<-median_stable_output
if(time_step=="MM" | time_step=="AM") outputdates1<-median_stable_output_date
if(time_step=="R" | time_step=="WM") outputseries1<-stable_output
if(time_step=="R" | time_step=="WM") outputdates1<-stable_output_date
if(calib_subset=="N") {outputseries1 <- outputseries1[which(outputdates1>calib_dates[1] & outputdates1<calib_dates[2])]; outputdates1 <- outputdates1[which(outputdates1>calib_dates[1] & outputdates1<calib_dates[2])]} # if the option to crop the output time series and only use a part of if for calibration has been chosen, the calibration vector is adapted accordingly. This option is only used for the stable isotopes, NOT for tritium.

x_stable<-1:length(inputseries1)
cal_dates1<-which(inputdates1 %in% outputdates1) #which dates are available as output measurements (used for the objective function)

if(exists("date_TTD")) {date_TTD_num <- decimal_date(as.Date(date_TTD)); index_datesel <- which(abs(inputdates1-date_TTD_num) == min(abs(inputdates1-date_TTD_num)))} # finds the entry of the input date vector closest to the chosen date for saving the transit time distribution

}

if(tracer=="H3+ST" | tracer=="H3") {
inputseries2<-h3_input # selects the input time series (tritium)
inputdates2<-h3_input_date
if(time_step=="MM" | time_step=="WM" | time_step=="AM") outputseries2<-median_h3_output
if(time_step=="MM" | time_step=="WM" | time_step=="AM") outputdates2<-median_h3_output_date
if(time_step=="R") outputseries2<-h3_output
if(time_step=="R") outputdates2<-h3_output_date
x_h3<-1:length(inputseries2)
cal_dates2<-which(inputdates2 %in% outputdates2)  #which dates are available as output measurements (used for the objective function)

if(exists("date_TTD")) {date_TTD_num <- decimal_date(as.Date(date_TTD)); index_datesel <- which(abs(inputdates2-date_TTD_num) == min(abs(inputdates2-date_TTD_num)))} # finds the entry of the input date vector closest to the chosen date for saving the transit time distribution
}


if(method=="SAS") {

dt_aggr <- sas_res  
data <- list()
data$dt <- dt_aggr
if(flow_dyn=="TRAN") data$J <- rain
if(flow_dyn=="TRAN") data$Q <- discharge
if(flow_dyn=="TRAN") data$ET <- etp
if(flow_dyn=="TRAN") data$wi <- wi
if(flow_dyn=="SS" & tracer=="ST") dummy=vector(mode="numeric",length=length(inputseries1))
if(flow_dyn=="SS" & tracer=="H3") dummy=vector(mode="numeric",length=length(inputseries2))
if(flow_dyn=="SS") data$J <- dummy+66 # if steady state calculations have been chosen, a dummy vector is created for all variables which stay constant over time (rainfall, discharge, evapotranspiration and weights). The values chosen for the fluxes correspond to an annual rainfall of 12*66=792 mm, discharge of 396 mm and evapotranspiration of 396 mm.
if(flow_dyn=="SS") data$Q <- dummy+33
if(flow_dyn=="SS") data$ET <- dummy+33
if(flow_dyn=="SS") data$wi <- dummy+0.5
rm(dummy)
#setwd(data_input_dir)
if(tracer=="ST") data1<-outputseries1
if(tracer=="ST") data$dates <- as.numeric(stable_input_date_SAS) #dates cannot be decimal dates for SAS
#setwd(SAS_dir)
if(tracer=="ST") data$C_J <- inputseries1*(-1) #for plotting reasons, isotope ratios should be positive
if(tracer=="ST") data$measC_Q <- outputseries1*(-1) #for plotting reasons, isotope ratios should be positive
if(tracer=="ST") data$indexC_Q <- match(outputdates1,inputdates1)

if(tracer=="H3") data1<-outputseries2
if(tracer=="H3") data$dates <- as.numeric(h3_input_date_SAS) #dates cannot be decimal dates for SAS
#setwd(SAS_dir)
if(tracer=="H3") data$C_J <- inputseries2 #for plotting reasons, isotope ratios should be positive
if(tracer=="H3") data$measC_Q <- outputseries2
if(tracer=="H3") data$indexC_Q <- match(outputdates2,inputdates2)


if(exists("date_TTD")) {date_TTD_num <- as.numeric(as.Date(date_TTD,"%Y-%m-%d")); data$index_datesel <- which(abs(data$dates-date_TTD_num) == min(abs(data$dates-date_TTD_num)))} # finds the entry of the input date vector closest to the chosen date for saving the transit time distribution
}


