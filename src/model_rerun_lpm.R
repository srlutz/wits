
        rerun<-dlgInput("Should a new calibration be performed (Y<- yes, N<- no)?", Sys.info()["user"])$res
        while (rerun!="Y" &  rerun!="N") {rerun<-dlgInput("The answer can only be one of the following: Y<- yes, N<- no", Sys.info()["user"])$res}

        if(rerun=="N") break
        

        
        modif<-dlgInput("What should be changed ? (algorithm <- ALGO, measure of fit <- MF, weighted months <- WGM, lpm model <- MOD, mtt range <- MTT, td <- TD, 2nd parameter <-2P, piston flow <- PF, calibration dates <- CALIB, ttd date <- TTDDATE, discretisation <- DIS, input weighting <- WIN, steady/unsteady flow <- FLOW )", Sys.info()["user"])$res
        while (modif!="ALGO" &  modif!="MF" &  modif!="WGM" &  modif!="MOD" &  modif!="MTT" &  modif!="TD" &  modif!="2P" &  modif!="PF"&  modif!="CALIB" & modif!="TTDDATE" & modif!="DIS" & modif!="WIN" & modif!="FLOW") {modif<-dlgInput("The answer can only be: algorithm <- ALGO, measure of fit <- MF, weighted months <- WGM, lpm model <- MOD, mtt range <- MTT, td range <- TD, 2nd parameter <-2P, piston flow <- PF, calibration dates <- CALIB, ttd date <- TTDDATE, discretisation <- DIS, input weighting <- WIN, steady/unsteady flow <- FLOW ", Sys.info()["user"])$res}

        
        if(modif=="ALGO"){
        calib<-dlgInput("Which algorithm should be used for calibration (BF<- brute force, GLUE<- GLUE)?", Sys.info()["user"])$res
        while (calib!="GLUE" &  calib!="BF") {calib<-dlgInput("The answer can only be one of the following: BF<- brute force, GLUE<- GLUE", Sys.info()["user"])$res}
        }


        if(modif=="MF"){
        obj_fun<-dlgInput("Which measure of fit should be used for calibration (SSE<- squared sum of error, NSSE<- Nash Sutcliffe, SSPE<- squared sum of prediction error, MPE<- mean prediction error )?", Sys.info()["user"])$res
        while (obj_fun!="SSE" &  obj_fun!="NSSE" &  obj_fun!="SSPE" &  obj_fun!="MPE") {obj_fun<-dlgInput("The answer can only be one of the following: SSE<- squared sum of error, NSSE<- Nash Sutcliffe, SSPE<- squared sum of prediction error, MPE<- mean prediction error", Sys.info()["user"])$res}
        }


        if(modif=="WGM"){
        if(weight_input!=1) weight_months<-dlgInput("Which months should be weighted (1<-January, 2<-February, etc..)? Enter each month number, separated by a comma. Normally 4,5,6,7,8,9", Sys.info()["user"])$res
        if(weight_input!=1) weight_months <- as.numeric(unlist(strsplit(weight_months, ",")))
        }

        
        if(modif=="MOD"){
        if(flow_dyn=="SS") lpm_model<-dlgInput("Which lumped parameter model should be used (EM<-exponential model, GM<-gamma model, DM<-dispersion model, DEM <- double exponential) ?", Sys.info()["user"])$res
        if(flow_dyn=="TRAN") lpm_model<-dlgInput("Which lumped parameter model should be used (EM<-exponential model, DM<-dispersion model)?", Sys.info()["user"])$res
        while (lpm_model!="EM" &  lpm_model!="GM" & lpm_model!="DM" & lpm_model!="DEM") {lpm_model<-dlgInput("The answer can only be one of the following: EM<-exponential model, GM<-gamma model, DM<-dispersion model", Sys.info()["user"])$res}
        
        if(lpm_model=="GM" | lpm_model=="DM" | lpm_model=="DEM") par2_range<-dlgInput("Give the minimum and maximum values for the second fitting parameter (gamma model:alpha, dispersion model: PD=1/Pe, double exp: relative weight of the shorter transit times) considered during model calibration. Enter two values, separated by a comma, for instance 0.1,1", Sys.info()["user"])$res
        if(lpm_model=="GM" | lpm_model=="DM" | lpm_model=="DEM") par2_range <- as.numeric(unlist(strsplit(par2_range, ",")))
        if(lpm_model=="GM" | lpm_model=="DM" | lpm_model=="DEM") {while(par2_range[1]<=0) {par2_range<-dlgInput("The second parameter cannot be equal to 0. Please enter an interval again.", Sys.info()["user"])$res; par2_range <- as.numeric(unlist(strsplit(par2_range, ",")))}}
        if(lpm_model=="DEM") {while(par2_range[2]>=1) {par2_range<-dlgInput("The second parameter cannot be equal to 1. Please enter an interval again.", Sys.info()["user"])$res; par2_range <- as.numeric(unlist(strsplit(par2_range, ",")))}}
        if(lpm_model=="DEM") {t_dem<-dlgInput("Give the values for the tdem parameter (in years) corresponding to the mean transit time of the quickflow reservoir of the double exponential model. This fitting parameter is not calibrated !", Sys.info()["user"])$res
t_dem <- as.numeric(unlist(strsplit(t_dem, ",")))
if(t_dem[1]<=0) {t_dem<-dlgInput("The t_dem parameter cannot be equal to 0. Please enter its value again.", Sys.info()["user"])$res; t_dem <- as.numeric(unlist(strsplit(t_dem, ",")))}
t_dem<-t_dem*time_unit} # changing the time unit of the t_dem parameter from years to the unit chosen by the user
        
        }
        
        
        
        if(modif=="MTT"){
        mtt_range<-dlgInput("Give the mean transit time range in years. Enter two values, separated by a comma, for instance 1,10 if the mean transit time is expected to be between 1 and 10 years", Sys.info()["user"])$res
        mtt_range <- as.numeric(unlist(strsplit(mtt_range, ",")))
        }

        if(modif=="TD"){
          td<-dlgInput("Give the values for the td parameter (in years) used for transient conditions and considered during model calibration. This parameter is not calibrated and must be estimated beforehand!", Sys.info()["user"])$res
          td <- as.numeric(unlist(strsplit(td, ",")))
          if(td[1]<=0) {td<-dlgInput("The td parameter cannot be equal to 0. Please enter its value again.", Sys.info()["user"])$res; td <- as.numeric(unlist(strsplit(td, ",")))}
          td<-td*time_unit
        }


        if(modif=="2P"){
          if(lpm_model=="GM" | lpm_model=="DM" | lpm_model=="DEM") par2_range<-dlgInput("Give the minimum and maximum values for the second fitting parameter (gamma model:alpha, dispersion model: PD=1/Pe, double exp: relative weight of the shorter transit times) considered during model calibration. Enter two values, separated by a comma, for instance 0.1,1", Sys.info()["user"])$res
          if(lpm_model=="GM" | lpm_model=="DM" | lpm_model=="DEM") par2_range <- as.numeric(unlist(strsplit(par2_range, ",")))
          if(lpm_model=="GM" | lpm_model=="DM" | lpm_model=="DEM") {while(par2_range[1]<=0) {par2_range<-dlgInput("The second parameter cannot be equal to 0. Please enter an interval again.", Sys.info()["user"])$res; par2_range <- as.numeric(unlist(strsplit(par2_range, ",")))}}
          if(lpm_model=="DEM") {while(par2_range[2]>=1) {par2_range<-dlgInput("The second parameter cannot be equal to 1. Please enter an interval again.", Sys.info()["user"])$res; par2_range <- as.numeric(unlist(strsplit(par2_range, ",")))}}
        }


        if(modif=="PF"){
          par4_range<-dlgInput("Give the minimum and maximum values for the piston-flow shift in years considered during model calibration. Enter two values, separated by a comma, for instance 0.1,1. If unnecessary, set the range to 0,0. A value of 0 means no piston-flow.", Sys.info()["user"])$res
          par4_range <- as.numeric(unlist(strsplit(par4_range, ",")))
        }


        if(modif=="CALIB"){
          calib_subset <- "N"
          if(calib_subset=="N") {calib_dates<-dlgInput("Indicate the first and last date for the calibration period separated by comma (format YYYY-mm-dd )", Sys.info()["user"])$res
          calib_dates <- decimal_date(as.Date(unlist(strsplit(calib_dates, ","))))}
          
        }


        if(modif=="TTDDATE"){
          if(flow_dyn=="TRAN") {
            date_TTD<-dlgInput("Give ONE date for which the transit time distribution should be plotted (yyyy-mm-dd)", Sys.info()["user"])$res
            date_TTD <- as.character(unlist(strsplit(date_TTD, ",")))
          }}


        if(modif=="DIS"){
          discretisation<-dlgInput("Give the values for the number of discretisation steps for used for each fitting parameter. 10 is the suggested minimum. Higher numbers increase dramatically calculation time!", Sys.info()["user"])$res
          discretisation <- as.numeric(unlist(strsplit(discretisation, ",")))
          if(discretisation[1]<10) {discretisation<-dlgInput("The discretisation steps should not be less than 10. Please enter a value >10.", Sys.info()["user"])$res; discretisation <- as.numeric(unlist(strsplit(discretisation, ",")))}
        }


        if(modif=="WIN"){
          weight_input<-dlgInput("Which weighting should be applied for the input (1<-no weighting, 2<-calculated from the shift in isotopic value between precipitation and discharge, [0;1]<-user defined (enter a value between 0 and 1))", Sys.info()["user"])$res
          while (weight_input<0 |  weight_input>1 & weight_input!=2) {weight_input<-dlgInput("The answer can only be between 0 and 1, or equal to 2", Sys.info()["user"])$res}
          weight_input <- as.numeric(weight_input)
          if(weight_input==1) weight_months<-c(1,2,3,4,5,6,7,8,9,10,11,12) # if the weighting option is not selected, all months are considered to contribute equally to the isotope input
          if(weight_input!=1) weight_months<-dlgInput("Which months should be weighted (1<-January, 2<-February, etc..)? Enter each month number, separated by a comma. Normally 4,5,6,7,8,9", Sys.info()["user"])$res
          if(weight_input!=1) weight_months <- as.numeric(unlist(strsplit(weight_months, ",")))
        }

        if(modif=="FLOW"){
        flow_dyn<-dlgInput("Should calculations be made for steady state or transient conditions (SS<- steady state, TRAN<- transient)?", Sys.info()["user"])$res
        while (flow_dyn!="SS" &  flow_dyn!="TRAN") {flow_dyn<-dlgInput("The answer can only be one of the following: SS<- steady state, TRAN<- transient", Sys.info()["user"])$res}
        if(flow_dyn=="TRAN") {td<-dlgInput("Give the values for the td parameter (in years) used for transient conditions and considered during model calibration. This parameter is not calibrated and must be estimated beforehand!", Sys.info()["user"])$res
        td <- as.numeric(unlist(strsplit(td, ",")))
        if(td[1]<=0) {td<-dlgInput("The td parameter cannot be equal to 0. Please enter its value again.", Sys.info()["user"])$res; td <- as.numeric(unlist(strsplit(td, ",")))}
        td<-td*time_unit} # changing the time unit of the td parameter from years to the unit chosen by the user
        if(flow_dyn=="TRAN") {
        date_TTD<-dlgInput("Give ONE date for which the transit time distribution should be plotted (yyyy-mm-dd)", Sys.info()["user"])$res
        date_TTD <- as.character(unlist(strsplit(date_TTD, ",")))
        }
        }



