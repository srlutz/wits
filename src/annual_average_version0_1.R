
### calculates the median monthly values for the isotope output and the monthly sum for water fluxes ###

# stable isotope

if(tracer=="ST" | tracer=="H3+ST" | weight_input==2) {

    dummy1=format(as.Date(date_decimal(stable_output_date)),"%Y") # grabs the dates for which output observations are available and removes the day and month so that annual data can be aggregated.
    median=aggregate(stable_output,list(dummy1),median) #calculates the median output concentration for each month with available data.
    dummy2=as.Date(paste(as.character(median$Group.1),"-01-15",sep=""),format="%Y-%m-%d") # adds the 15th January of each month to the date vector to match the date of the input entries.
    median_stable_output=median$x # vector with median monthly output value for the stable isotope.
    median_stable_output_date=decimal_date(dummy2) # vector with the dates of the calculated median values.
    stable_output_date_SAS=as.Date(dummy2) # vector with the dates of the calculated median values.
    rm(dummy1,dummy2,median)

}

# tritium

if(tracer=="H3" | tracer=="H3+ST") {

    dummy1=format(as.Date(date_decimal(h3_output_date)),"%Y") # grabs the dates for which output observations are available and removes the day and month so that annual data can be aggregated.
    median=aggregate(h3_output,list(dummy1),median) #calculates the median output concentration for each month with available data.
    dummy2=as.Date(paste(as.character(median$Group.1),"-01-15",sep=""),format="%Y-%m-%d") # adds the 1st January of each month to the date vector to match the date of the input entries.
    median_h3_output=median$x # vector with median monthly output value for the stable isotope.
    median_h3_output_date=decimal_date(dummy2) # vector with the dates of the calculated median values.

}

# discharge
if(flow_dyn=="TRAN") {

    ts_date <- discharge_date
    ts_val <- discharge
    dummy1=format(as.Date(date_decimal(h3_output_date)),"%Y") # grabs the dates for which output observations are available and removes the day and month so that annual data can be aggregated.
    median=aggregate(h3_output,list(dummy1),sum) #calculates the sum of all fluxes for each year with available data.
    dummy2=as.Date(paste(as.character(median$Group.1),"-01-15",sep=""),format="%Y-%m-%d") # adds the 1st January of each month to the date vector to match the date of the input entries.
    dummy3 <- median$x # vector with median monthly input value for the stable isotope.
    x1 <- dummy2[1]
    x2 <- dummy2[length(dummy2)]
    x_interp <- decimal_date(seq(x1,x2,by="year")) # creates a continuous date vector with one entry on the 15th of each month given in decimal numbers
    x_ts <- decimal_date(dummy2)
    prov <- approx(x_ts,dummy3,x_interp,method = "linear") # linear interpolation of the vector dummy3 with dates interpolated at points x
    discharge <- round(prov$y,digits=2) # extracts the interpolated results and replaces the original observation vector with the interpolated one.
    discharge_date <- x_interp

    rm(dummy1,dummy2,dummy3,median,x1,x2,x_interp,x_ts,prov)

}


