if(manual_input=="N")
{
# choice 1: Defines which method will be used (LPM or SAS)
method<-dlgInput("Which method should be used to relate input and output (LPM <- lumped parameter model, SAS <- storage selection functions)?", Sys.info()["user"])$res
while (method!="LPM" &  method!="SAS") {method<-dlgInput("The answer can only be one of the following: LPM <- lumped parameter model, SAS <- stream selection functions", Sys.info()["user"])$res}

# choice 2: which isotope(s) should be used as tracer, and which stable isotope (if applicable)?
if(method=="LPM") {tracer<-dlgInput("Which tracers should be used for calibration (H3<- tritium alone, ST<- stable isotope alone, H3+ST<-both)?", Sys.info()["user"])$res
while (tracer!="H3" &  tracer!="ST" &  tracer!="H3+ST") {tracer<-dlgInput("The answer can only be one of the following: H3<- tritium alone, ST<- stable isotope alone, H3+ST<-both", Sys.info()["user"])$res}}

if(method=="SAS") {tracer<-dlgInput("Which tracer should be used for calibration (H3<- tritium, ST<- stable isotope)?", Sys.info()["user"])$res
while (tracer!="H3" &  tracer!="ST") {tracer<-dlgInput("The answer can only be one of the following: H3<- tritium, ST<- stable isotope", Sys.info()["user"])$res}} # when using SAS, only one tracer at a time can be modelled, as otherwise calculation time explodes.

if (tracer=="ST" | tracer=="H3+ST") {stable_iso<-dlgInput("Which stable isotope (O18<- oxygen-18, H2<- deuterium)?", Sys.info()["user"])$res;while (stable_iso!="O18" &  stable_iso!="H2") {stable_iso<-dlgInput("The answer can only be one of the following: O18<- oxygen-18, H2<- deuterium", Sys.info()["user"])$res}}

# choice 3: algorithm used for automatic calibration. NOTE: Only "brute force" has been kept, thus, the choice is commented out. GLUE is NOT operational.
calib <- "BF"
#calib<-dlgInput("Which algorithm should be used for calibration (BF<- brute force, GLUE<- GLUE)?", Sys.info()["user"])$res
#while (calib!="GLUE" &  calib!="BF") {calib<-dlgInput("The answer can only be one of the following: BF<- brute force, GLUE<- GLUE", Sys.info()["user"])$res}

# choice 4: objective function used to calculate the goodness of fit
obj_fun<-dlgInput("Which measure of fit should be used for calibration (NSE<- Nash Sutcliffe, MPE<- mean prediction error )?", Sys.info()["user"])$res
while (obj_fun!="SSE" &  obj_fun!="NSE" &  obj_fun!="SSPE" &  obj_fun!="MPE") {obj_fun<-dlgInput("The answer can only be one of the following: NSE<- Nash Sutcliffe, MPE<- mean prediction error", Sys.info()["user"])$res}

# choice 5: time steps and duration used for modelling. If calculations are done without calculating a monthly or weekly mean, the user has to make sure that the time step is the same for all input and output time series.
time_step<-dlgInput("Should the calibration be performed using yearly, monthly or weekly means or using the raw output values (AM <- yearly means, MM <- monthly means, WM<-weekly means, R <- raw values )?", Sys.info()["user"])$res
while (time_step!="AM" &time_step!="MM" &  time_step!="R"&  time_step!="WM") {time_step<-dlgInput("The answer can only be one of the following: A <- yearly means, MM <- monthly means, WM<-weekly means, R <- raw values", Sys.info()["user"])$res}
time_unit<-dlgInput("What is the duration of each input time step (1 <- annual time steps, 12 <- monthly time steps, 52 <- weekly time steps, 365 <- daily time steps)", Sys.info()["user"])$res
if(time_step!="R") {while (!(time_unit=="1" |  time_unit=="12" | time_unit=="52" | time_unit=="365")) {time_unit<-dlgInput("The answer can only be 1, 12, 52 or 365", Sys.info()["user"])$res}} # checks whether the time unit is onle of the allowed values. This check is disabled if the option "raw value" is chosen, as in that case, the user can enter another value.
time_unit <- as.numeric(time_unit) # Define the duration of each time step (used to scale the calibration parameters having units of time and for tritium decay). 


# choice 6: Time window used for calibration

calib_subset<-dlgInput("Should the calibration be performed using the entire isotope time series (Y <- yes, N <- no )? ", Sys.info()["user"])$res
while (calib_subset!="Y" &  calib_subset!="N") {calib_subset<-dlgInput("The answer can only be one of the following: Y <- yes, N <- no ", Sys.info()["user"])$res}

if(calib_subset=="N") {calib_dates<-dlgInput("Indicate the first and last date for the calibration period separated by comma (format YYYY-mm-dd )", Sys.info()["user"])$res
calib_dates <- decimal_date(as.Date(unlist(strsplit(calib_dates, ","))))}

# choice 7: discretisation of the parameter space

discretisation<-dlgInput("Give the values for the number of discretisation steps for used for each fitting parameter. 10 is the suggested minimum. Higher numbers increase dramatically calculation time!", Sys.info()["user"])$res
discretisation <- as.numeric(unlist(strsplit(discretisation, ",")))
if(discretisation[1]<10) {discretisation<-dlgInput("The discretisation steps should not be less than 10. Please enter a value >10.", Sys.info()["user"])$res; discretisation <- as.numeric(unlist(strsplit(discretisation, ",")))}

}


if(manual_input=="Y")
{
# choice 1: Defines which method will be used (LPM or SAS)

method="LPM" # Which method should be used to relate input and output (LPM <- lumped parameter model, SAS <- storage selection functions)?

# choice 2: which isotope(s) should be used as tracer, and which stable isotope (if applicable)?
tracer<-"H3" # Which tracers should be used for calibration (H3<- tritium alone, ST<- stable isotope alone, H3+ST<-both)?

stable_iso<-"O18" # Which stable isotope (O18<- oxygen-18, H2<- deuterium)?

# choice 3: algorithm used for automatic calibration
calib<-"BF" #Which algorithm should be used for calibration (BF<- brute force, GLUE<- GLUE). NOTE: Only "brute force" has been kept. GLUE is NOT operational.

# choice 4: objective function used to calculate the goodness of fit
obj_fun<-"MPE" #Which measure of fit should be used for calibration (NSE<- Nash Sutcliffe, MPE<- mean prediction error )?

# choice 5: time steps and duration used for modelling. If calculations are done without calculating a monthly or weekly mean, the user has to make sure that the time step is the same for all input and output time series.
time_step<-"MM" # Should the calibration be performed using the monthly or weekly means or using the raw input values (AM <- yearly means, MM <- monthly means, WM<-weekly means, R <- raw values )?
time_unit<-12 # Define the duration of each input time step (used mainly for tritium decay). 1 <- annual time steps, 12 <- monthly time steps, 52 <- weekly time steps, 365 <- daily time steps

# choice 6: Time window used for calibration
calib_subset<-"Y" # Should the calibration be performed using the entire isotope time series (Y <- yes, N <- no )?

calib_dates<-c("1977-01-01","1978-06-01")
calib_dates <- decimal_date(as.Date(unlist(strsplit(calib_dates, ","))))

# choice 7: discretisation of the parameter space

discretisation <- 10 #Give the values for the number of discretisation steps for used for each fitting parameter. 10 is the suggested minimum. Higher numbers increase dramatically calculation time!

}

