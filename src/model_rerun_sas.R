     choice <- function(){
          x <- readline("Should a new calibration be performed (Y<- yes, N<- no)? ")  
          if(x!="Y" &  x!="N") readline("The answer can only be one of the following: Y<- yes, N<- no ") else x <- x
        }
        rerun<-choice()

        if(rerun=="N") break
        
        choice <- function(){
          x <- readline("What should be changed (algorithm <- ALGO, measure of fit <- MF, weighting <- WG, weighted months <- WGM, model <- MOD, initial storage <- ISTORE, initial tracer value <- INISO, calibration dates <- CALIB, ttd date <- TTDDATE, discretisation <- DIS")  
          if(x!="ALGO" &  x!="MF" &  x!="WG" &  x!="WGM" &  x!="MOD" & x!="ISTORE" & x!="INISO" & x!="CALIB" & x!="TTDDATE" & modif!="DIS") readline("The answer can only be: algorithm <- ALGO, measure of fit <- MF, weighting <- WG, weighted months <- WGM, model <- MOD, initial storage <- ISTORE, initial tracer value <- INISO, calibration dates <- CALIB, ttd date <- TTDDATE, discretisation <- DIS ") else x <- x
        }
        modif<-choice()

        if(modif=="ALGO"){
                choice <- function(){
                  x <- readline("Which algorithm should be used for calibration (BF<- brute force, GLUE<- GLUE)? ")  
                  if(x!="GLUE" &  x!="BF") readline("The answer can only be one of the following: BF<- brute force, GLUE<- GLUE ") else x <- x
                }
                calib<-choice()
            }

        if(modif=="MF"){
            choice <- function(){
                  x <- readline("Which measure of fit should be used for calibration (NSE<- Nash Sutcliffe, MPE<- mean prediction error )? ")  
                  if(x!="SSE" &  x!="NSE" &  x!="SSPE" &  x!="MPE") readline("The answer can only be one of the following: SSE<- squared sum of error, NSE<- Nash Sutcliffe, SSPE<- squared sum of prediction error, MPE<- mean prediction error ") else x <- x
                 }
                obj_fun<-choice()
            }

        if(modif=="WG"){
                choice <- function(){
                  x <- readline("Which weighting should be applied for the input (1<-no weighting, 2<-calculated from the shift in isotopic value between precipitation and discharge, [0;1]<-user defined (enter a value between 0 and 1))?")  
                  x <- as.numeric(x)
                  if(x<0 |  x>1 & x!=2) x=readline("The answer can only be between 0 and 1, or equal to 2 ") else x <- x
                  x <- as.numeric(x)
                }
                weight_input<-choice()
            }

        if(modif=="WGM"){
                choice <- function(){
                  x <- readline("Which months should be weighted (1<-January, 2<-February, etc..)? Enter each month number, separated by a comma. Normally 4,5,6,7,8,9 ")  
                  x <- as.numeric(unlist(strsplit(x, ",")))
                }
                weight_months<-choice() # the months to be weighted 
            }

        if(modif=="MOD"){
                choice <- function(){
                  x <- readline("Which SAS function should be used? Enter 1, 2, or 3 (1: fixed power-law, 2: time-variant power-law, 3: beta shape) ")  
                  if(x!=1 &  x!=2 & x!=3) readline("The answer can only be one of the following: 1: fixed power-law, 2: time-variant power-law, 3: beta shape ") else x <- x
                }
                sas_fun <- choice()

                choice <- function(){
                  x <- readline("Give min und max values for the intial storage in [mm] to search the optimal solution, separated by comma ")  
                  x <- as.numeric(unlist(strsplit(x, ",")))
                }
                stor_range <- choice()

                choice <- function(){
                  x <- readline("Give min und max values for k_Q (discharge flux) to search the optimal solution, separated by comma ")  
                  x <- as.numeric(unlist(strsplit(x, ",")))
                }
                if(sas_fun==1) kQ_range <- choice() # this option is only activated if model 1 is selected.

                choice <- function(){
                  x <- readline("Give min und max values for k_ET (ET flux) to search the optimal solution, separated by comma ")  
                  x <- as.numeric(unlist(strsplit(x, ",")))
                }
                if(sas_fun==1) kET_range <- choice() # this option is only activated if model 1 is selected.

                choice <- function(){
                  x <- readline("Give min und max values for kmin_Q to search the optimal solution, separated by comma ")  
                  x <- as.numeric(unlist(strsplit(x, ",")))
                }
                if(sas_fun==2) kminQ_range <- choice() # this option is only activated if model 2 is selected.

                choice <- function(){
                  x <- readline("Give min und max values for kmax_Q to search the optimal solution, separated by comma ")  
                  x <- as.numeric(unlist(strsplit(x, ",")))
                }
                if(sas_fun==2) kmaxQ_range <- choice() # this option is only activated if model 2 is selected.

                choice <- function(){
                  x <- readline("Give min und max values for beta_ET to search the optimal solution, separated by comma ")  
                  x <- as.numeric(unlist(strsplit(x, ",")))
                }
                if(sas_fun==2) betaET_range <- choice() # this option is only activated if model 2 is selected.


                choice <- function(){
                  x <- readline("Give min und max values for alpha to search the optimal solution, separated by comma ")  
                  x <- as.numeric(unlist(strsplit(x, ",")))
                }
                if(sas_fun==3) alpha_range <- choice() # this option is only activated if model 3 is selected.

                choice <- function(){
                  x <- readline("Give min und max values for beta to search the optimal solution, separated by comma ")  
                  x <- as.numeric(unlist(strsplit(x, ",")))
                }
                if(sas_fun==3) beta_range <- choice() # this option is only activated if model 3 is selected.

                choice <- function(){
                  x <- readline("Give min und max values for beta_ET to search the optimal solution, separated by comma ")  
                  x <- as.numeric(unlist(strsplit(x, ",")))
                }
                if(sas_fun==3) betaET_range <- choice() # this option is only activated if model 3 is selected.
            }


        if(modif=="INISO"){
                choice <- function(){
                  x <- readline("Give the initial tracer value of the catchment storage (in permille. Must be a number!) ")  
                  x <- as.numeric(x)*(-1)
                }
                sas_ini<-choice() # this option is only activated if a transient model is selected.
            }

        if(modif=="ISTORE"){
                choice <- function(){
                  x <- readline("Give min und max values for the intial storage in [mm] to search the optimal solution, separated by comma ")  
                  x <- as.numeric(unlist(strsplit(x, ",")))
                }
                stor_range <- choice() # this option is only activated if model 3 is selected.
            }

        if(modif=="CALIB"){
                calib_subset="N"
                choice <- function(){
                  x <- readline("Indicate the first and last date for the calibration period separated by comma (format YYYY-mm-dd) ")
                  x <- decimal_date(as.Date(unlist(strsplit(x, ","))))
                }
                calib_dates<-choice()
            }

        if(modif=="TTDDATE"){
                choice <- function(){
                  x <- readline("Give ONE date for which transit times should be plotted (yyyy-mm-dd)")
                  x <- as.character(unlist(strsplit(date_TTD, ",")))
                }
                date_TTD <- choice()
            }

        if(modif=="DIS"){
          discretisation<-dlgInput("Give the values for the number of discretisation steps for used for each fitting parameter. 10 is the suggested minimum. Higher numbers increase dramatically calculation time!", Sys.info()["user"])$res
          discretisation <- as.numeric(unlist(strsplit(discretisation, ",")))
          if(discretisation[1]<10) {discretisation<-dlgInput("The discretisation steps should not be less than 10. Please enter a value >10.", Sys.info()["user"])$res; discretisation <- as.numeric(unlist(strsplit(discretisation, ",")))}
        }



