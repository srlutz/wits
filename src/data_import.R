
setwd(data_input_dir)

# tritium flux into the system
#data<-read.csv("tritium_trier_vienna_1961_2019.csv",header=TRUE,na.strings<-"#N/A") # Tritium input function.
if(tracer=="H3" | tracer=="H3+ST") {
data<-read.csv("h3_input.csv",header=TRUE,sep=",") # Tritium input function.
#data<-na.omit(data)
h3_input<-data$H3
h3_input_date <- decimal_date(as.Date(data$Date,"%Y-%m-%d")) # converts the dates to decimal numbers
#h3_input_date <- decimal_date(as.Date(data$Date,"%m/%j/%Y")) # converts the dates to decimal numbers
}

# stable isotope flux into the system
#data<-read.csv("h2_trier_1978_2013.csv",header=TRUE,na.strings<-"#N/A") # Input function for the stable isotope.
if(tracer=="ST" | tracer=="H3+ST" | weight_input==2) {
data<-read.csv("stable_input.csv",header=TRUE,sep=",") # Input function for the stable isotope.
#if (method=="LPM") {data<-read.csv("stable_input.csv",header=TRUE,sep=",")} # Input function for the stable isotope.
#if (method=="SAS") {data<-read.csv("stable_input_sas.csv",header=TRUE,sep=",")} #Input function for SAS TEST
data<-na.omit(data)
stable_input<-data$isotope
stable_input_date <- decimal_date(as.Date(data$Date,"%Y-%m-%d")) # converts the dates to decimal numbers.
stable_input_date_SAS<- as.numeric(as.Date(data$Date,"%Y-%m-%d")) #imports the dates for SAS, as decimal numbers are not working in SAS
}

# tritium flux out of the system
#data<-read.csv("Weierbach_tritium_2011-2017.csv",header=TRUE,na.strings<-"#N/A") #tritium measurements at the outlet of the system under consideration.
if(tracer=="H3" | tracer=="H3+ST") {
data<-read.csv("h3_output.csv",header=TRUE,sep=",") #tritium measurements at the outlet of the system under consideration.
error_bars <- 0 # by default, it is assumed that no error bars will be plotted with the output measurements for tritium
if("error" %in% colnames(data)) {error_bars <- 1; data$error[is.na(data$error)]=0} # if a column with the analytical error is present in the original data set, the switch for plotting the error bars is flipped on, and any missing error value is replaced by "0"
data<-na.omit(data)
h3_output<-data$H3
h3_output_date<-decimal_date(as.Date(data$Date,format="%Y-%m-%d")) # converts the dates to decimal numbers
if(error_bars==1) {h3_error <- data$error} # if a column with the analytical error is present in the original data set, it is added as additional object and further used in the plots to visualise the error bars
}

# stable isotope flux out of the system
if(tracer=="ST" | tracer=="H3+ST" | weight_input==2) {
data<-read.csv("stable_output.csv",header=TRUE,sep=",") #stable isotope measurements in spring water
#if (method=="LPM") {data<-read.csv("stable_output.csv",header=TRUE,sep=",")} #stable isotope measurements in spring water
#if (method=="SAS") {data<-read.csv("stable_output_sas.csv",header=TRUE,sep=",")} # Input function for SAS TEST
data<-na.omit(data)
stable_output<-data$isotope
stable_output_date<-decimal_date(as.Date(data$Date,format="%Y-%m-%d")) # converts the dates to decimal numbers
stable_output_date_SAS<- as.numeric(as.Date(data$Date,"%Y-%m-%d")) 
}

# water flux out of the system (in case a variable flow formulation is used)
if(flow_dyn=="TRAN") {
data<-read.csv("discharge.csv",header=TRUE,sep=",") 
data<-na.omit(data)
discharge<-data$Q
discharge_date<-decimal_date(as.Date(data$Date,format="%Y-%m-%d")) # converts the dates to decimal numbers
}

# additional input data rainfall, evapotranspiration and weight for SAS model 

if(file.exists("rainfall.csv")==TRUE) { # if rainfall data has been provided, it is automatically loaded and used to weight the input function
data<-read.csv("rainfall.csv",header=TRUE,sep=",") 
#data<-na.omit(data)
rain<-data$J
rain_date<-decimal_date(as.Date(data$Date,format="%Y-%m-%d")) # converts the dates to decimal numbers
}

if(method=="SAS" & flow_dyn=="TRAN") {
data<-read.csv("rainfall.csv",header=TRUE,sep=",") 
#data<-na.omit(data)
rain<-data$J
rain_date<-decimal_date(as.Date(data$Date,format="%Y-%m-%d")) # converts the dates to decimal numbers
}

if(method=="SAS" & flow_dyn=="TRAN") {
data<-read.csv("evapotranspiration.csv",header=TRUE,sep=",") 
#data<-na.omit(data)
etp<-data$etp
etp_date<-decimal_date(as.Date(data$Date,format="%Y-%m-%d")) # converts the dates to decimal numbers
}

if(method=="SAS" & flow_dyn=="TRAN") {
data<-read.csv("weights.csv",header=TRUE,sep=",") 
#data<-na.omit(data)
wi<-data$wi
wi_date<-decimal_date(as.Date(data$Date,format="%Y-%m-%d")) # converts the dates to decimal numbers
}

setwd(routines_dir)

