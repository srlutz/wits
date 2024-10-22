# Perform a monte carlo brute force simulation

BFrun1 <- function(model,inputseries,outputseries,time.period,p1,p2,p3,p4,obj_fun,output_dir) {
  
  rm(list=ls())
  graphics.off()
 
 # creates a matrix with all parameter combinations in a given range
    if(p1[1]==p1[2])  {dummy1=seq(p1[1],p1[2],length.out=1)} else {dummy1=seq(p1[1],p1[2],length.out=discretisation)} # if the offset shift should not be fitted, it is set to the one value given by the user.
    if(p2[1]==p2[2])  {dummy2=seq(p2[1],p2[2],length.out=1)} else {dummy2=seq(p2[1],p2[2],length.out=discretisation)} # if the mean transit time should not be fitted, it is set to the one value given by the user.
    #dummy1=seq(p1[1],p1[2],length.out=10)
    #dummy2=seq(p2[1],p2[2],length.out=10)
    if(model=="EM" | lpm_model=="EPM") {dummy3=seq(p3[1],p3[2],length.out=1)} else {dummy3=seq(p3[1],p3[2],length.out=discretisation)} # if the second LPM fitting parameter is not needed, it is set to "0" and only one value is generated in the parameter combination object
    if(p4[1]==0 & p4[2]==0)  {dummy4=seq(p4[1],p4[2],length.out=1)} else {dummy4=seq(p4[1],p4[2],length.out=discretisation)} # if the piston flow component is not needed, it is set to "0" and only one value is generated in the parameter combination object
    #if(p4[2]==1 & time_unit==1)  {dummy4=seq(p4[1],p4[2],length.out=2)} # if the second piston flow component is equal to 1 for annual input time steps, only two values equal to piston flow 0 years and piston flow 1 year are used.
    if(time_unit==1)  {dummy4=seq(p4[1],p4[2],by=1)} # if a piston flow component is desired with annual time steps, it is set to whole numbers between the given minimum and maximum. 

    params<-do.call(expand.grid,list(dummy1,dummy2,dummy3,dummy4)) #parameters that are reinitialised at each run of the loop
    names(params)<- c("p1","p2","p3","p4")
  ############################################
  
  nparam <- 4 #length(Extra$min.param) # number of parameters - for LPMs, we will always have 4
  
  # trim observed output to desired time period --> not needed?
  obs <-outputseries#[Extra$time.period[1]:Extra$time.period[2]]
  
  out_df <- data.frame(err=double(),param1=double(),param2=double(),param3=double(),param4=double()) # object storing the model parameters of each run
  outiso_df <- data.frame(matrix(ncol = length(inputseries), nrow = 0)) # object storing the modelling results of each run
  outresid_df <- data.frame(matrix(ncol = length(obs), nrow = 0)) # object storing the residuals of each run (observed minus predicted)
  out_ttd <- data.frame(matrix(ncol = length(inputseries), nrow = 0)) # object storing the modelling results of each run
  
  p1<- params$p1
  if(tracer=="H3+ST" & decay==1) p1 <- p1*0 # sets the offset to "0" if this is the tritium run of the dual isotope analysis, for which the parameter combinations of the stable isotope are reused.
  p2<- params$p2
  p3<- params$p3
  p4<- params$p4

  ############################################
  
  # start the loop of the iterations
  for(ii in 1:dim(params)[1]){
    if(run==1) print(paste(100*(ii/dim(params)[1]),"% - calibration-1st tracer"))
    if(run==2) print(paste(100*(ii/dim(params)[1]),"% - calibration-2nd tracer"))
    # select one parameter set from the matrix containing all combinations
    param <- c(p1[ii],p2[ii],p3[ii],p4[ii])

    # compute the output depending on model choice and parameters

    # the transfer function is selected, and used for the convolution

    if(model=="GM") {
      obshat <- do.call("GM", args = list(inputseries=inputseries+param[1],x=1:length(inputseries),par1=param[2]*time_unit,par3=param[3],par4=round(param[4]*time_unit))) # the piston flow component is rounded to an integer to obtain a whole time that can be used easily to shift the time series.
    }

    if(model=="EM") {
      obshat <- do.call("EM", args = list(inputseries=inputseries+param[1],x=1:length(inputseries),par1=param[2]*time_unit,par4=round(param[4]*time_unit))) # the piston flow component is rounded to an integer to obtain a whole time that can be used easily to shift the time series.
    }

    if(model=="DM") {
      obshat <- do.call("DM", args = list(inputseries=inputseries+param[1],x=1:length(inputseries),par1=param[2]*time_unit,par3=param[3],par4=round(param[4]*time_unit))) # # the piston flow component is rounded to an integer to obtain a whole time that can be used easily to shift the time series.
    }

    if(model=="DEM") {
      obshat <- do.call("DEM", args = list(inputseries=inputseries+param[1],x=1:length(inputseries),par1=param[2]*time_unit,par3=param[3],par4=round(param[4]*time_unit))) # # the piston flow component is rounded to an integer to obtain a whole time that can be used easily to shift the time series.
    }


    output_LPM=as.numeric(as.character(unlist(obshat[1])))
    EM_dist=as.numeric(as.character(unlist(obshat[2]))) # saves transit time distribution, either steady state or for variable flow. In the latter case, one date only is exported, as defined by the user
    #iso_mod <- output_LPM[time.period[1]:time.period[2]] # saves predictions
    iso_mod <- output_LPM[time.period] # saves predictions
    resid_mod <- iso_mod-obs # saves residuals
    # compute different objective functions
    NSE <- max(0 , 1 - (sum((obs-iso_mod)^2)) / (sum((obs-mean(obs))^2))) # Nash-Sutcliff
    MPE=sqrt(sum((iso_mod-obs)^2))/length(obs) # mean prediction error
    if (obj_fun=="NSE") err=NSE   
    if (obj_fun=="MPE") err=MPE  

    #append output for ii-th simulation
    out_df<-rbind(out_df,data.frame("err"=err,"param1"=param[1],"param2"=param[2],"param3"=param[3],"param4"=param[4])) # param1=horizontal shift of the output time series, param2=mean transit time, param3=additional fitting parameter for GM and DM (alpha or PD), param4=piston flow shift)
    outiso_df<-rbind(outiso_df,output_LPM)
    outresid_df<-rbind(outresid_df,resid_mod)
    out_ttd<-rbind(out_ttd,EM_dist)

  }
  #update colnames in simulated data
  dates <- paste("D", 1:length(inputseries), sep="")
  colnames(outiso_df)<-dates 
  
  write.csv(out_df,file=output,row.names=F)
  write.csv(outiso_df,file=output_sim,row.names=F)
  write.csv(outresid_df,file=resid_sim,row.names=F)
  write.csv(out_ttd,file=output_ttd,row.names=F)
  if (tracer=="H3+ST") write.csv(out_ttd,file=output_ttd2,row.names=F)
  
}
