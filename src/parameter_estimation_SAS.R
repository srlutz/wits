
q1=0.05 # quantiles threshold of all simulations used to find the best fits (0.05 means the best 5% of all simulations are retained)
if(obj_fun=="NSE") q1=0.95

# stable isotope

if (tracer=="ST" | tracer=="H3+ST") {

    output_MC <- read.csv(paste(output_dir,"/Output_stable.txt",sep="")) # load output
    output_mod <- read.csv(paste(output_dir,"/sim_Output_stable.txt",sep=""))
    resid_mod <- read.csv(paste(output_dir,"/sim_residuals_stable.txt",sep=""))
    ix<-which(output_MC$err==min(output_MC$err)) # finds the best fit
    if(obj_fun=="NSE")     ix<-which(output_MC$err==max(output_MC$err)) # finds the best fit
    if(length(ix)>1) ix <- ix[1] # security in case more than one parameter combination leads to the same measure of fit (because the discretisation was set too high, so that one parameter is in a range where results are virtually the same). In that case, only the first solution is kept for further processing.
    q2<-quantile(output_MC$err,probs=q1) # quantiles of all stable isotope simulations used to find the best fits (0.1 means the best 10% of all simulations are retained)
    ix2<-which(output_MC$err<q2) # finds solutions with measures of fit better than the chosen percentile threshold
   if(obj_fun=="NSSE"| obj_fun=="SSE")    ix2<-which(output_MC$err>q2) # finds solutions with measures of fit better than the chosen percentile threshold
    # prints the parameters of the best fit
    if(sas_fun == 1 & tracer=="ST" ) print(paste("Best solution for the stable isotope measurements is: error ",round(output_MC[ix,1],2),", kQ <- ", round(output_MC[ix,2],2),", kET <- ", round(output_MC[ix,3],2)," and storage <- ", round(output_MC[ix,4],2), " mm.", sep=""))
    if(sas_fun == 2 & tracer=="ST" ) print(paste("Best solution for the stable isotope measurements is: error ",round(output_MC[ix,1],2),", kQmin <- ", round(output_MC[ix,2],2),", kQmax <- ", round(output_MC[ix,3],2),", betaET <- ", round(output_MC[ix,4],2)," and storage <- ", round(output_MC[ix,5],2), " mm.", sep=""))
    if(sas_fun == 3 & tracer=="ST" ) print(paste("Best solution for the stable isotope measurements is: error ",round(output_MC[ix,1],2),", alpha <- ", round(output_MC[ix,2],2),", beta <- ", round(output_MC[ix,3],2),", betaET <- ", round(output_MC[ix,4],2)," and storage <- ", round(output_MC[ix,5],2), " mm.", sep=""))

    y_stable<-output_mod[ix,] # saves best prediction
    y_stable_residuals<- resid_mod[ix,] # saves the residuals of the best prediction
    ensemble_stable <- output_mod[ix2,] # saves ensemble of best solutions
    params_best_stable_ensemble<- output_MC[ix2,] # saves the parameters of the best fits
    params_best_stable<- output_MC[ix,1:5] # saves the parameters of the absolute best fit
    params_stable<- output_MC # saves all parameter combinations
    source("ttd_SAS.R")

    res1=data.frame(inputdates1,unlist(y_stable))
    names(res1)=c("date","value")
    res2=data.frame(outputdates1,unlist(y_stable_residuals))
    names(res2)=c("date","value")
    res3=data.frame(params_best_stable)
    if(sas_fun==1) names(res3)=c("error","kQ","kET","storage")
    if(sas_fun==2) names(res3)=c("error","kQmin","kQmax","betaET","storage")
    if(sas_fun==3) names(res3)=c("error","alpha","beta","betaET","storage")
    best_fit_stable_ts <- paste(output_dir,"/Output_best_fit_stable.txt",sep="") # output file name for best fit
    best_fit_stable_residuals <- paste(output_dir,"/Output_best_fit_stable_residuals.txt",sep="") # output file name for best fit residuals
    best_fit_stable_residuals_parameters <- paste(output_dir,"/best_fit_stable_parameters.txt",sep="") # output file name for best fit residuals
    write.csv(res1,file=best_fit_stable_ts,row.names=F)
    write.csv(res2,file=best_fit_stable_residuals,row.names=F)
    write.csv(res3,file=best_fit_stable_residuals_parameters,row.names=F)
    write.csv(TTD,file=TTD_save,row.names=F)
    }

# tritium

if (tracer=="H3" | tracer=="H3+ST") {

    output_MC <- read.csv(paste(output_dir,"/Output_h3.txt",sep="")) # load output
    output_mod <- read.csv(paste(output_dir,"/sim_Output_h3.txt",sep=""))
    resid_mod <- read.csv(paste(output_dir,"/sim_residuals_h3.txt",sep=""))
    if(tracer=="H3+ST") { # if both tracers are used for calibration, the duplicated parameter combinations due to the offset being set to "0" for tritium have to be removed first, as otherwise, the best fit will appear multiple times
    dummy=which(duplicated(output_MC)==T)
    output_MC <- output_MC[-dummy,]
    output_mod <- output_mod[-dummy,]
    resid_mod <- resid_mod[-dummy,]
    ttd_mod <- ttd_mod[-dummy,]
    rm(dummy)
    }

    ix<-which(output_MC$err==min(output_MC$err)) # finds the best fit
    if(obj_fun=="NSSE"| obj_fun=="SSE")     ix<-which(output_MC$err==max(output_MC$err)) # finds the best fit
    if(length(ix)>1) ix <- ix[1] # security in case more than one parameter combination leads to the same measure of fit (because the discretisation was set too high, so that one parameter is in a range where results are virtually the same). In that case, only the first solution is kept for further processing.
    q2=quantile(output_MC$err,probs=q1) # quantiles of all stable isotope simulations used to find the best fits (0.1 means the best 10% of all simulations are retained)
    ix2<-which(output_MC$err<q2) # finds solutions with measures of fit better than the chosen percentile threshold
    if(obj_fun=="NSSE"| obj_fun=="SSE")    ix2<-which(output_MC$err>q2) # finds solutions with measures of fit better than the chosen percentile threshold
    # prints the parameters of the best fit
    if(sas_fun == 1 & tracer=="H3" ) print(paste("Best solution for tritium measurements is: error ",round(output_MC[ix,1],2),", kQ <- ", round(output_MC[ix,2],2),", kET <- ", round(output_MC[ix,3],2)," and storage <- ", round(output_MC[ix,4],2), " mm.", sep=""))
    if(sas_fun == 2 & tracer=="H3" ) print(paste("Best solution for tritium measurements is: error ",round(output_MC[ix,1],2),", kQmin <- ", round(output_MC[ix,2],2),", kQmax <- ", round(output_MC[ix,3],2),", betaET <- ", round(output_MC[ix,5],2)," and storage <- ", round(output_MC[ix,4],2), " mm.", sep=""))
    if(sas_fun == 3 & tracer=="H3" ) print(paste("Best solution for tritium measurements is: error ",round(output_MC[ix,1],2),", alpha <- ", round(output_MC[ix,2],2),", beta <- ", round(output_MC[ix,3],2),", betaET <- ", round(output_MC[ix,4],2)," and storage <- ", round(output_MC[ix,5],2), " mm.", sep=""))

    y_h3<-output_mod[ix,] # saves best prediction
    y_h3_residuals<- resid_mod[ix,] # saves the residuals of the best prediction
    ensemble_h3 <- output_mod[ix2,] # saves ensemble of best solutions
    params_best_h3_ensemble<- output_MC[ix2,] # saves the parameters of the best fits
    params_best_h3<- output_MC[ix,1:5] # saves the parameters of the absolute best fit
    params_h3<- output_MC # saves all parameter combinations
    source("ttd_SAS.R")

    res1=data.frame(inputdates2,unlist(y_h3))
    names(res1)=c("date","value")
    res2=data.frame(outputdates2,unlist(y_h3_residuals))
    names(res2)=c("date","value")
    res3=data.frame(params_best_h3)
    if(sas_fun==1) names(res3)=c("error","kQ","kET","storage")
    if(sas_fun==2) names(res3)=c("error","kQmin","kQmax","betaET","storage")
    if(sas_fun==3) names(res3)=c("error","alpha","beta","betaET","storage")
    best_fit_h3_ts <- paste(output_dir,"/Output_best_fit_h3.txt",sep="") # output file name for best fit
    best_fit_h3_residuals <- paste(output_dir,"/Output_best_fit_h3_residuals.txt",sep="") # output file name for best fit residuals
    best_fit_h3_residuals_parameters <- paste(output_dir,"/best_fit_h3_parameters.txt",sep="") # output file name for best fit residuals
    write.csv(res1,file=best_fit_h3_ts,row.names=F)
    write.csv(res2,file=best_fit_h3_residuals,row.names=F)
    write.csv(res3,file=best_fit_h3_residuals_parameters,row.names=F)
    write.csv(TTD,file=TTD_save,row.names=F)
    }

# both tracers

if (tracer=="H3+ST") {

    output_MC1 <- read.csv(paste(output_dir,"/Output_h3.txt",sep="")) # load output
    output_mod1 <- read.csv(paste(output_dir,"/sim_Output_h3.txt",sep=""))
    resid_mod1 <- read.csv(paste(output_dir,"/sim_residuals_h3.txt",sep=""))
    output_MC2 <- read.csv(paste(output_dir,"/Output_stable.txt",sep="")) # load output
    output_mod2 <- read.csv(paste(output_dir,"/sim_Output_stable.txt",sep=""))
    resid_mod2 <- read.csv(paste(output_dir,"/sim_residuals_stable.txt",sep=""))

    q2=quantile(output_MC1$err,probs=q1) # quantiles of all tritium simulations used to find the best fits (0.1 means the best 10% of all simulations are retained)
    q3=quantile(output_MC2$err,probs=q1) # quantiles of all stable isotope simulations used to find the best fits (0.1 means the best 10% of all simulations are retained)
    ix=which(output_MC1$err<q2 & output_MC2$err<q3) # finds solutions for both tracers with measures of fit better than the xx percentile
    if(obj_fun=="NSSE"| obj_fun=="SSE")       ix=which(output_MC1$err>q2 & output_MC2$err>q3) # finds solutions for both tracers with measures of fit better than the xx percentile
    if(length(ix)==0) { # if no solution can be found, the quantile threshold is increased incrementally until at least one solution remains
    if(obj_fun=="SSPE"| obj_fun=="MPE") q1=q1+0.05
    if(obj_fun=="NSSE"| obj_fun=="SSE") q1=q1-0.05
    q2=quantile(output_MC1$err,probs=q1)
    q3=quantile(output_MC2$err,probs=q1)
    ix=which(output_MC1$err<q2 & output_MC2$err<q3)
    if(obj_fun=="NSSE"| obj_fun=="SSE")       ix=which(output_MC1$err>q2 & output_MC2$err>q3) # finds solutions for both tracers with measures of fit better than the xx percentile
    if(length(ix)==0) break
    }

    ix2=which(output_MC1$err[ix]==min(output_MC1$err[ix])) # finds the best tritium fit amongst the best compromise solutions
    ix3=ix[ix2]

    # prints the parameters of the best fit
    if(sas_fun == 1 & tracer=="ST" ) print(paste("Best solution for the stable isotope measurements is: error ",round(output_MC[ix3,1],2),", kQ <- ", round(output_MC[ix3,2],2),", kET <- ", round(output_MC[ix3,3],2)," and storage <- ", round(output_MC[ix3,4],2), " mm.", sep=""))
    if(sas_fun == 2 & tracer=="ST" ) print(paste("Best solution for the stable isotope measurements is: error ",round(output_MC[ix3,1],2),", kQmin <- ", round(output_MC[ix3,2],2),", kQmax <- ", round(output_MC[ix3,3],2),", betaET <- ", round(output_MC[ix3,4],2)," and storage <- ", round(output_MC[ix3,5],2), " mm.", sep=""))
    if(sas_fun == 3 & tracer=="ST" ) print(paste("Best solution for the stable isotope measurements is: error ",round(output_MC[ix3,1],2),", alpha <- ", round(output_MC[ix3,2],2),", beta <- ", round(output_MC[ix3,3],2),", betaET <- ", round(output_MC[ix3,4],2)," and storage <- ", round(output_MC[ix3,5],2), " mm.", sep=""))


    y_combined_stable<-output_mod2[ix3,]+mean(inputseries1) # saves best prediction
    y_combined_stable_residuals<- resid_mod2[ix3,] # saves the residuals of the best prediction
    ensemble_combined_stable <- output_mod2[ix,]+mean(inputseries1) # saves ensemble of best solutions
    params_combined_best_stable_ensemble<- output_MC2[ix,] # saves the parameters of the best fits
    params_combined_best_stable<- output_MC2[ix3,1:5] # saves the parameters of the absolute best fit
    params_combined_stable<- output_MC2 # saves all parameter combinations
    source("ttd_SAS.R")
    TTD1 <- TTD

    y_combined_h3<-output_mod1[ix3,] # saves best prediction
    y_combined_h3_residuals<- resid_mod1[ix3,] # saves the residuals of the best prediction
    ensemble_combined_h3 <- output_mod1[ix,] # saves ensemble of best solutions
    params_best_combined_h3_ensemble<- output_MC1[ix,] # saves the parameters of the best fits
    params_best_combined_h3<- output_MC1[ix3,1:5] # saves the parameters of the absolute best fit
    params_combined_h3<- output_MC1 # saves all parameter combinations
    source("ttd_SAS.R")
    TTD2 <- TTD

    res1=data.frame(inputdates1,unlist(y_combined_stable))
    names(res1)=c("date","value")
    res2=data.frame(outputdates1,unlist(y_combined_stable_residuals))
    names(res2)=c("date","value")
    res3=data.frame(params_combined_best_stable)
    if(sas_fun==1) names(res3)=c("error","kQ","kET","storage")
    if(sas_fun==2) names(res3)=c("error","kQmin","kQmax","betaET","storage")
    if(sas_fun==3) names(res3)=c("error","alpha","beta","betaET","storage")
    best_fit_stable_ts <- paste(output_dir,"/Output_best_fit_combined_stable.txt",sep="") # output file name for best fit
    best_fit_stable_residuals <- paste(output_dir,"/Output_best_fit_combined_stable_residuals.txt",sep="") # output file name for best fit residuals
    best_fit_stable_residuals_parameters <- paste(output_dir,"/best_fit_combined_stable_parameters.txt",sep="") # output file name for best fit residuals
    write.csv(res1,file=best_fit_stable_ts,row.names=F)
    write.csv(res2,file=best_fit_stable_residuals,row.names=F)
    write.csv(res3,file=best_fit_stable_residuals_parameters,row.names=F)
    write.csv(TTD1,file=TTD_save1,row.names=F)

    res1=data.frame(inputdates2,unlist(y_combined_h3))
    names(res1)=c("date","value")
    res2=data.frame(outputdates2,unlist(y_combined_h3_residuals))
    names(res2)=c("date","value")
    res3=data.frame(params_best_combined_h3)
    if(sas_fun==1) names(res3)=c("error","kQ","kET","storage")
    if(sas_fun==2) names(res3)=c("error","kQmin","kQmax","betaET","storage")
    if(sas_fun==3) names(res3)=c("error","alpha","beta","betaET","storage")
    best_fit_h3_ts <- paste(output_dir,"/Output_best_fit_combined_h3.txt",sep="") # output file name for best fit
    best_fit_h3_residuals <- paste(output_dir,"/Output_best_fit_combined_h3_residuals.txt",sep="") # output file name for best fit residuals
    best_fit_h3_residuals_parameters <- paste(output_dir,"/best_fit_combined_h3_parameters.txt",sep="") # output file name for best fit residuals
    write.csv(res1,file=best_fit_h3_ts,row.names=F)
    write.csv(res2,file=best_fit_h3_residuals,row.names=F)
    write.csv(res3,file=best_fit_h3_residuals_parameters,row.names=F)
    write.csv(TTD2,file=TTD_save2,row.names=F)

    }

