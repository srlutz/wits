
#### runs the convolutions for all parameter combinations #####


# first for one tracer only

if(calib=="GLUE") {
    run <- 1
    source("GLUErun1.R")
    model<-lpm_model 
    time.period <- c(cal_dates) # vector flagging the entries of the simulated time series for which observations are available for calibration
    GLUErun1(model,inputseries,outputseries,time.period,p1,p2,p3,p4,nit,obj_fun,output_dir)      
    }

if(calib=="BF") {
    run <- 1
    source("BFrun1.R")
    model<-lpm_model 
    time.period <- c(cal_dates) # vector flagging the entries of the simulated time series for which observations are available for calibration
    BFrun1(model,inputseries,outputseries,time.period,p1,p2,p3,p4,obj_fun,output_dir)
    }


# if chosen by the user, a second tracer is then used for convolution

if (tracer=="H3+ST") { # In that case, the pseudo-random parameter combinations defined for the GLUE run of the stable isotope OR the brute force parameter vectors are used to calculate the convolutions for tritium.

    inputseries<-inputseries2
    outputseries<-outputseries2
    cal_dates<-cal_dates2
    x<-x_h3 # idem for the time vector used for convolution
    if(flow_dyn=="TRAN") Q <- discharge[which(discharge_date==inputdates2[1]):which(discharge_date==inputdates2[length(inputdates2)])] # selects the discharge measurements for the time period of available isotope input for variable flow calculations
    decay=1 # switch for the radioactive decay. 0=no decay (stable isotope), 1=tritium decay
    output <- paste(output_dir,"/Output_h3.txt",sep="") # output file name for objective functions
    output_sim<- paste(output_dir,"/sim_Output_h3.txt",sep="") # output file name for simulated isotope output
    resid_sim<- paste(output_dir,"/sim_residuals_h3.txt",sep="") # output file name for the residuals
    # computes decay losses for tritium
    DT.trit=12.3 #tritium half-life [y]
    DT.trit=DT.trit*time_unit #tritium half-life [units of the chosen time steps]
    decay.const=log(2)/DT.trit
    decay.vec=exp(-decay.const*x) #computes decay losses 

    if(calib=="GLUE") {


        dummy1 <- read.csv(paste(output_dir,"/Output_stable.txt",sep="")) # load output
        p1=dummy1$param1 # offset shift of the stable isotope output
        p2=dummy1$param2 # mean transit time
        p3=dummy1$param3 # additional transfer function parameter
        p4=dummy1$param4 # piston flow
        p5=rep(0,length=length(dummy1$param5)) # offset shift of the tritium output. Set equal to '0' for the moment.
        #p5=dummy1$param5 # offset shift of the tritium output
        run <- 2
        source("GLUErun2.R")
        model<-lpm_model 
        time.period <- c(cal_dates) # vector flagging the entries of the simulated time series for which observations are available to calculated the goodness of fit.
        GLUErun2(model,inputseries,outputseries,time.period,p1,p2,p3,p4,p5,nit,obj_fun)
          
                   }


    if(calib=="BF") {

        p1 <- offset_range
        p2 <- mtt_range
        if(lpm_model=="EM" | lpm_model=="EPM") { p3<- c(0,0) } else {p3 <- par2_range}
        p4 <- par4_range
        run <- 2
        source("BFrun1.R")
        model<-lpm_model 
            #if(model == "EM") p3<- c(0,0)  # range of the second fitting parameter set to "0" in the case of the exponential model (the only fitting parameter being the mean transit time). This is an additional security in case the user has forgotten to set the range to "0" in the parameter definition section.
        time.period <- c(cal_dates) # vector flagging the entries of the simulated time series for which observations are available to
        BFrun1(model,inputseries,outputseries,time.period,p1,p2,p3,p4,obj_fun,output_dir)
          
                   }

}

