# This routine calculates the summer infiltration coefficient used as weighting factor for the input functions based on the approach proposed by Grabczak et al., Catena 11, 1984, 105-114. NOTE: if no rainfall data is provided, variable monthly precipitation amounts are ignored in the estimation of the infiltration coefficient.

if(exists("rain")==F) rain<-rep(1,length(stable_input)) # creates a synthetic rainfall vector if none is provided. The vector is populated with "ones".

 # creating a vector flagging the summer months  over the entire observations period
dummy1<-weight_months # months of the year defined as "summer months", either by default (i.e. month 4,5,6,7,8 and 9) or by the user
dummy2=as.numeric(format(as.Date(date_decimal(stable_input_date)),"%m")) # extracts the months from the data vector of input
dummy3=which(dummy2 %in% dummy1) # finds the entries in the input vector that should be weigthed (normally, the summer months)

# calculating the weighting factor

if(weight_input==2) {

    c_ground<-mean(stable_output)   
    # implementation of equation 5 in Grabczak et al.
    diff1 <- mean(stable_input[-dummy3])-c_ground
    diff2 <- c_ground-mean(stable_input[dummy3])
    numerator <- diff1*sum(rain[-dummy3]) # Winter months are selected (those assumed to contribute fully to the isotopic flux)
    denominator <- diff2*sum(rain[-dummy3]) # Summer months are selected (those assumed to contribute only partially to the isotopic flux)
    alpha<-abs(numerator/denominator) # winter to summer infiltration coefficient. For instance, 0.8 means that the summer isotopic contribution to the total output signal is 80% of winter contribution (which is supposed to be 100% of isotopic rainfall of the winter months).
    if(alpha>1) {print("The calculated summer to winter ratio is >1, which is physically improbable. Alpha is set to 1 instead. The user is advised to check the input data.",quote=F); alpha=1; readline(prompt="Press [enter] to proceed")}
    if(stable_iso=="O18") lab_error=0.1 else lab_error=1 # sets the analytical measurement error for the chosen stable isotope (1 per mille for oxygen-18 and 0.1 per mille for deuterium)
    if((abs(diff1)+abs(diff2))<lab_error) alpha <- 1 # overrides the alpha calculated and replaces it by 1 (i.e. no weighting) if the difference between sommer and winter isotopic mean is less than the analytical error. Otherwise, small values can artificially yield low alpha coefficients, whereas the small difference between sommer and winter means actually indicate the opposite.
    rm(numerator,denominator)
}

if(weight_input>0 & weight_input<1) {alpha<-weight_input} # sets alpha to a user defined value between 0 (no summer contribution) and 1 (all months contribute equally)
if(weight_input==1 | time_unit==1) {alpha<-1} # sets the alpha value to 1 if the user has decided not to apply a weighting factor or if annual means are used for the input (i.e. alpha is equal to one, hence both winter and summer infiltration contribute completely to the isotopic influx)

# Weighting the measured inputs by the estimated infiltration coefficient

if(tracer=="ST" | tracer=="H3+ST") {

    sum_weights<-sum(rain[dummy3]*alpha)+sum(rain[-dummy3])
    N<-length(stable_input)
    sum_weights<-sum(rain[dummy3]*alpha)+sum(rain[-dummy3])
    if(weight_input!=1) stable_input[dummy3]<-c_ground+(N*alpha*rain[dummy3]*(stable_input[dummy3]-c_ground)/sum_weights) # brings the isotopic signal of the summer month closer to the long term annual mean in groundwater. Equation 9 in Maloszewski et al., Journal of Hydrology 140 (1992), 343-360, assuming alpha_winter=1, so that alpha_summer=alpha*alpha_winter=alpha.
    rm(N,sum_weights)

}

if(tracer=="H3" | tracer=="H3+ST") {

    h3_input<-h3_input #tritium 

    # creating a vector flagging the summer months  over the entire observations period
    h3_input[dummy3] <- alpha*h3_input[dummy3] # multiplies the summer months by the alpha coefficient.

}

rm(dummy1,dummy2,dummy3)

