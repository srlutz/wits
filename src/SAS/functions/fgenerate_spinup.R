#Note: Make sure to replace 'your_data', 'your_period_rep', and 'your_n_rep' with your actual data, period, and repetition values. 
#Also, note that the indexing in R starts from 1, whereas in MATLAB, it starts from 0.


fgenerate_spinup <- function(data, period_rep, n_rep) {
  # Identify the dataset to be repeated
  index_rep <- seq(floor((period_rep[1] - 1) * (24 / data$dt) + 1),
                   floor(period_rep[2] * (24 / data$dt)))
  
  # Index of timesteps that will be repeated
  data$ini_shift <- length(index_rep) * n_rep  # Total number of timesteps to add at the beginning
  
  # Plug the spinup at the beginning of the datasets
  new_dates <- seq(from = (data$dates[1] - data$ini_shift * data$dt / 24),
                   by = data$dt / 24, length.out = data$ini_shift)
  data$dates <- c(new_dates, data$dates)
  data$J <- c(rep(data$J[index_rep], each = n_rep), data$J)
  data$ET <- c(rep(data$ET[index_rep], each = n_rep), data$ET)
  data$Q <- c(rep(data$Q[index_rep], each = n_rep), data$Q)
  data$C_J <- c(rep(data$C_J[index_rep], each = n_rep), data$C_J)
  data$wi <- c(rep(data$wi[index_rep], each = n_rep), data$wi)
  
  # Shift the indexes for C_Q and age
  data$indexC_Q <- data$indexC_Q + data$ini_shift  # Update measurement dates index
  data$index_datesel <- data$index_datesel + data$ini_shift  # Update index with dates for age computation
  
  return(data)
}

# Example usage:
# Assuming 'your_data' is the data structure in R similar to 'data' in MATLAB
# and 'your_period_rep' and 'your_n_rep' are the corresponding values.
# your_data <- fgenerate_spinup(your_data, your_period_rep, your_n_rep)
