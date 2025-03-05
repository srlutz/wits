#library("tidyverse")


#we do gap filling after the time discritization is defined by the user. Makes our lives easier.


#yearly means
if (time_unit==1){ 
if(tracer=="ST" | tracer=="H3+ST") { #this is only required for LPM and is independent of weighting (could just be erased)
ts_date <- stable_input_date
ts_val <- stable_input
dummy1=format(as.Date(date_decimal(ts_date)),"%Y") # grabs the dates for which output observations are available and removes the day so that monthly data can be aggregated.
median=aggregate(ts_val,list(dummy1),median) #calculates the median output concentration for each month with available data.
dummy2=as.Date(paste(as.character(median$Group.1),"-01-15",sep=""),format="%Y-%m-%d") # adds the 15th of each January to the date vector to match the date of the input entries.
dummy3 <- median$x # vector with median monthly input value for the stable isotope.
x1 <- dummy2[1]
x2 <- dummy2[length(dummy2)]
x_interp <- decimal_date(seq(x1,x2,by="year")) # creates a continuous date vector with one entry on the 15th of each month given in decimal numbers
x_ts <- decimal_date(dummy2)
#gap filling
prov <- approx(x_ts,dummy3,x_interp,method = "linear") # linear interpolation of the vector dummy3 with dates interpolated at points x
stable_input_raw <- stable_input # saves the original time series
stable_input <- round(prov$y,digits=2) # extracts the interpolated results and replaces the original observation vector with the interpolated one.
stable_input_date_raw <- stable_input_date
stable_input_date <- x_interp
stable_input_date_SAS <- as.numeric(as.Date(date_decimal(stable_input_date)))

 }
# Tritium 
#if(method=="LPM"|tracer=="H3" | tracer=="H3+ST") {
if(tracer=="H3" | tracer=="H3+ST") {
ts_date <- h3_input_date
ts_val <- h3_input
dummy1=format(as.Date(date_decimal(ts_date)),"%Y") # grabs the dates for which output observations are available and removes the day so that monthly data can be aggregated.
median=aggregate(ts_val,list(dummy1),median) #calculates the median output concentration for each year with available data.
dummy2=as.Date(paste(as.character(median$Group.1),"-01-15",sep=""),format="%Y-%m-%d") # adds the 15th of each January to the date vector to match the date of the input entries.
dummy3 <- median$x # vector with median yearly input value for the stable isotope.
x1 <- dummy2[1]
x2 <- dummy2[length(dummy2)]
x_interp <- decimal_date(seq(x1,x2,by="year")) # creates a continuous date vector with one entry on the 15th of each month given in decimal numbers
x_ts <- decimal_date(dummy2)
#gap filling
prov <- approx(x_ts,dummy3,x_interp,method = "linear") # linear interpolation of the vector dummy3 with dates interpolated at points x
h3_input_raw <- h3_input # saves the original time series
h3_input <- round(prov$y,digits=2) # extracts the interpolated results and replaces the original observation vector with the interpolated one.
h3_input_date_raw <- h3_input_date
h3_input_date <- x_interp
h3_input_date_SAS <- as.numeric(as.Date(date_decimal(h3_input_date)))

 }

}


#monthly means

if (time_unit==12){ 

if(tracer=="ST" | tracer=="H3+ST") { #this is only required for LPM and is independent of weighting (could just be erased)
ts_date <- stable_input_date
ts_val <- stable_input
dummy1=format(as.Date(date_decimal(ts_date)),"%Y-%m") # grabs the dates for which output observations are available and removes the day so that monthly data can be aggregated.
median=aggregate(ts_val,list(dummy1),median) #calculates the median output concentration for each month with available data.
dummy2=as.Date(paste(as.character(median$Group.1),"-15",sep=""),format="%Y-%m-%d") # adds the 15th of each month to the date vector to match the date of the input entries.
dummy3 <- median$x # vector with median monthly input value for the stable isotope.
x1 <- dummy2[1]
x2 <- dummy2[length(dummy2)]
x_interp <- decimal_date(seq(x1,x2,by="month")) # creates a continuous date vector with one entry on the 15th of each month given in decimal numbers
x_ts <- decimal_date(dummy2)
#gap filling
prov <- approx(x_ts,dummy3,x_interp,method = "linear") # linear interpolation of the vector dummy3 with dates interpolated at points x
stable_input_raw <- stable_input # saves the original time series
stable_input <- round(prov$y,digits=2) # extracts the interpolated results and replaces the original observation vector with the interpolated one.
stable_input_date_raw <- stable_input_date
stable_input_date <- x_interp
stable_input_date_SAS <- as.numeric(as.Date(date_decimal(stable_input_date)))
 }

# Tritium 
#if(method=="LPM"|tracer=="H3" | tracer=="H3+ST") {
if(tracer=="H3" | tracer=="H3+ST") {
ts_date <- h3_input_date
ts_val <- h3_input
dummy1=format(as.Date(date_decimal(ts_date)),"%Y-%m") # grabs the dates for which output observations are available and removes the day so that monthly data can be aggregated.
median=aggregate(ts_val,list(dummy1),median) #calculates the median output concentration for each month with available data.
dummy2=as.Date(paste(as.character(median$Group.1),"-15",sep=""),format="%Y-%m-%d") # adds the 15th of each month to the date vector to match the date of the input entries.
dummy3 <- median$x # vector with median monthly input value for the stable isotope.
x1 <- dummy2[1]
x2 <- dummy2[length(dummy2)]
x_interp <- decimal_date(seq(x1,x2,by="month")) # creates a continuous date vector with one entry on the 15th of each month given in decimal numbers
x_ts <- decimal_date(dummy2)
#gap filling
prov <- approx(x_ts,dummy3,x_interp,method = "linear") # linear interpolation of the vector dummy3 with dates interpolated at points x
h3_input_raw <- h3_input # saves the original time series
h3_input <- round(prov$y,digits=2) # extracts the interpolated results and replaces the original observation vector with the interpolated one.
h3_input_date_raw <- h3_input_date
h3_input_date <- x_interp
h3_input_date_SAS <- as.numeric(as.Date(date_decimal(h3_input_date)))

 }

}

#weekly mean ("WM")

#stable isotopes
if(time_unit==52){ 

  if(tracer=="ST" | tracer=="H3+ST") { #this is only required for LPM and is independent of weighting (could just be erased)
    setwd(data_input_dir)
    dummy_data<-read.csv("stable_input.csv",header=TRUE,sep=",") # here i have to load the isotope data again as the algorithm has difficulties to extract weeks from decimal dates. This we should adjust in the near future.
    dummy_data<-na.omit(dummy_data)
    dummy_data$Date_num <- as.numeric(as.Date(dummy_data$Date,"%Y-%m-%d"))
    dummy_data$Date <- as.Date(dummy_data$Date,"%Y-%m-%d")
    # Add a week column to the data frame
    dummy_data <- dummy_data %>%
      mutate(week = floor_date(Date, "week"))
    # Calculate weekly averages
    weekly_averages <- dummy_data %>%
      group_by(week) %>%
      summarize(average_value = median(isotope))
    #create weekly timeseries from start to end date of measurement period (always sunday defines the end of a week in R)
    weekly_dates <- seq.Date(from = min(dummy_data$Date), to = max(dummy_data$Date), by = "week")
    #gap filling if gap exists
    prov <- approx(weekly_averages$week,weekly_averages$average_value,weekly_dates,method = "linear")
    stable_input_raw <- stable_input # saves the original time series
    stable_input <- round(prov$y,digits=2) # extracts the interpolated results and replaces the original observation vector with the interpolated one.
    stable_input_date_raw <- stable_input_date
    stable_input_date <- decimal_date(weekly_dates) #now the weekly means are assigned to the sunday in each week 
    stable_input_date_SAS <- as.numeric(as.Date(date_decimal(stable_input_date)))
    rm(weekly_averages,weekly_dates,prov,dummy_data)
}

# Tritium

  if(tracer=="H3" | tracer=="H3+ST") {
    setwd(data_input_dir)
    dummy_data<-read.csv("h3_input.csv",header=TRUE,sep=",") # here i have to load the tritium data again as the algorithm has difficulties to extract weeks from decimal dates. This we should adjust in the near future.
    dummy_data<-na.omit(dummy_data)
    dummy_data$Date_num <- as.numeric(as.Date(dummy_data$Date,"%Y-%m-%d"))
    dummy_data$Date <- as.Date(dummy_data$Date,"%Y-%m-%d")
    # Add a week column to the data frame
    dummy_data <- dummy_data %>%
      mutate(week = floor_date(Date, "week"))
    # Calculate weekly averages
    weekly_averages <- dummy_data %>%
      group_by(week) %>%
      summarize(average_value = median(isotope))
    #create weekly timeseries from start to end date of measurement period (always sunday defines the end of a week in R)
    weekly_dates <- seq.Date(from = min(dummy_data$Date), to = max(dummy_data$Date), by = "week")
    #gap filling if gap exists
    prov <- approx(weekly_averages$week,weekly_averages$average_value,weekly_dates,method = "linear")
    h3_input_raw <- h3_input # saves the original time series
    h3_input <- round(prov$y,digits=2) # extracts the interpolated results and replaces the original observation vector with the interpolated one.
    h3_input_date_raw <- h3_input_date
    h3_input_date <- decimal_date(weekly_dates) #now the weekly means are assigned to the sunday in each week 
    h3_input_date_SAS <- as.numeric(as.Date(date_decimal(h3_input_date)))
    rm(weekly_averages,weekly_dates,prov,dummy_data)
  }
}
#daily timeseries- 
# the user himself needs to provide a complete isotope timeseries as any interpolation from our side would introduce too much assumptions, which we don´t want to be involved in!!
  
rm(dummy1,dummy2,dummy3,median,x1,x2,x_interp,x_ts,prov)

setwd(routines_dir)

