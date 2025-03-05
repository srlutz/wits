
#--------------------------------------------------------------------------
# assigns the parameter vectors depending on the chosen method
#--------------------------------------------------------------------------

if (method=="LPM") {
    p1 <- offset_range # vertical offset of the output time series
    if(tracer=="H3") p1 <- c(0,0) # the vertical offset of the output time series is set to "0" in the case of tritium
    p2 <- mtt_range # mean transit time in the units chosen by the user
    if(lpm_model=="EM" | lpm_model=="EPM") { p3<- c(0,0) } else {p3 <- par2_range} # second lumped parameter model parameter, if required
    p4 <- par4_range # piston-flow in the units chosen by the user
    }


if (method=="SAS") {

    if(sas_fun==1) {
        p1 <- kQ_range # k_Q value
        p2 <- kET_range # k_ET value
        p3 <- stor_range # initial storage [mm]
        p5 <- offset_range # vertical offset of the output time series
        if(tracer=="H3") p5 <- c(0,0) # the vertical offset of the output time series is set to "0" in the case of tritium
    }

    if (sas_fun==2) {
      p1 <- kminQ_range # kmin_Q value
      p2 <- kmaxQ_range # kmax_Q value
      p3 <- betaET_range # beta_ET value
      p4 <- stor_range # initial storage [mm]
      p5 <- offset_range # vertical offset of the output time series
      if(tracer=="H3") p5 <- c(0,0) # the vertical offset of the output time series is set to "0" in the case of tritium
    }
     
    if (sas_fun==3) {
      p1 <- alpha_range # alpha value
      p2 <- beta_range # beta value
      p3 <- betaET_range # beta_ET value
      p4 <- stor_range # initial storage [mm]
      p5 <- offset_range # vertical offset of the output time series
      if(tracer=="H3") p5 <- c(0,0) # the vertical offset of the output time series is set to "0" in the case of tritium
    } 
    if (tracer=="ST" | tracer=="H3+ST") { 
        output <- paste(output_dir,"/Output_stable.txt",sep="") # output file name for objective functions
        output_sim<- paste(output_dir,"/sim_Output_stable.txt",sep="") # output file name for simulated isotope output
        resid_sim<- paste(output_dir,"/sim_residuals_stable.txt",sep="") # output file name for the residuals
        output_ttd<- paste(output_dir,"/sim_ttd_stable.txt",sep="") # output file name for the transit time distribution
    }

    if (tracer=="H3") { 
        output <- paste(output_dir,"/Output_h3.txt",sep="") # output file name for objective functions
        output_sim<- paste(output_dir,"/sim_Output_h3.txt",sep="") # output file name for simulated isotope output
        resid_sim<- paste(output_dir,"/sim_residuals_h3.txt",sep="") # output file name for the residuals
        output_ttd<- paste(output_dir,"/sim_ttd_h3.txt",sep="") # output file name for the transit time distribution
    }
 
}

if (method=="LPM" | method=="SAS") {

#--------------------------------------------------------------------------
# stable isotope data
#--------------------------------------------------------------------------

if (tracer=="ST" | tracer=="H3+ST") {

    inputseries<-inputseries1-mean(inputseries1) # selects the input time series. inputseries1<- stable isotope, inputseries2<- tritium. The time series is shifted by the mean input value to centre it around 0 (numerical trick to shorten the warm up period, as otherwise, the predicted output must first sink to the mean value from 0)
    outputseries<-outputseries1-mean(inputseries1) # idem for the output time series
    cal_dates<-cal_dates1 # idem for the calibration dates
    x<-x_stable # idem for the time vector used for convolution
    if(flow_dyn=="TRAN") Q <- discharge[which(discharge_date==inputdates1[1]):which(discharge_date==inputdates1[length(inputdates1)])] # selects the discharge measurements for the time period of available isotope input for variable flow calculations
    decay=0 # switch for the radioactive decay. 0=no decay (stable isotope), 1=tritium decay
    # selects the discharge measurements for the time period of available isotope input for variable flow calculations
    output <- paste(output_dir,"/Output_stable.txt",sep="") # output file name for objective functions
    output_sim<- paste(output_dir,"/sim_Output_stable.txt",sep="") # output file name for simulated isotope output
    resid_sim<- paste(output_dir,"/sim_residuals_stable.txt",sep="") # output file name for the residuals
    output_ttd<- paste(output_dir,"/sim_ttd_stable.txt",sep="") # output file name for the transit time distribution
    if (tracer=="H3+ST") output_ttd2<- paste(output_dir,"/sim_ttd_h3.txt",sep="") # output file name for the transit time distribution
    TTD_save <- paste(output_dir,"/TTD.txt",sep="") # output file name for best fit of the transit time distribution
    if (tracer=="H3+ST") {TTD_save1 <- paste(output_dir,"/TTD_stable.txt",sep="");TTD_save2 <- paste(output_dir,"/TTD_h3.txt",sep="");TTD_save3 <- paste(output_dir,"/TTD_stable_compromise.txt",sep="");TTD_save4 <- paste(output_dir,"/TTD_h3_compromise.txt",sep="")}
}

#--------------------------------------------------------------------------  
# tritium data
#--------------------------------------------------------------------------

if (tracer=="H3") {

    inputseries<-inputseries2 # selects the input time series. inputseries1<- stable isotope, inputseries2<- tritium
    outputseries<-outputseries2 # idem for the output time series
    cal_dates<-cal_dates2 # idem for the calibration dates
    x<-x_h3 # idem for the time vector used for convolution
    if(flow_dyn=="TRAN") Q <- discharge[which(discharge_date==inputdates2[1]):which(discharge_date==inputdates2[length(inputdates2)])] # selects the discharge measurements for the time period of available isotope input for variable flow calculations
    decay=1 # switch for the radioactive decay. 0=no decay (stable isotope), 1=tritium decay
    output <- paste(output_dir,"/Output_h3.txt",sep="") # output file name for objective functions
    output_sim<- paste(output_dir,"/sim_Output_h3.txt",sep="") # output file name for simulated isotope output
    resid_sim<- paste(output_dir,"/sim_residuals_h3.txt",sep="") # output file name for the residuals
    output_ttd<- paste(output_dir,"/sim_ttd_h3.txt",sep="") # output file name for the transit time distribution
    TTD_save <- paste(output_dir,"/TTD.txt",sep="") # output file name for best fit of the transit time distribution
    # computes decay losses for tritium
    DT.trit=12.3 #tritium half-life [y]
    DT.trit=DT.trit*time_unit #tritium half-life [units of the chosen time steps]
    decay.const=log(2)/DT.trit
    decay.vec=exp(-decay.const*x) #computes decay losses 
    if(method=="LPM") p1 <- c(0,0) # no offset for tritium, as the weighting should be sufficient
 
                    }

}
