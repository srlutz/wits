Eff_SAS <- function(obs, mod) {
  
  # Settings
  display_eff <- TRUE  # Flag to print efficiencies: TRUE = yes, FALSE = no
  
  # Check if obs and mod have the same length
  if (length(obs) != length(mod)) {
    warning('modeled and observed data have different lengths')
  }
  
  # Remove possible NaNs from measurements and corresponding model values
  nonan <- !is.na(obs)
  obs <- obs[nonan]
  mod <- mod[nonan]
  
  # Some useful quantities
  measvar <- var(obs)         # Measurement variance
  err <- obs - mod            # Residual
  
  # Mean Error
  merr <- mean(abs(err))
  
  # Nash-Sutcliffe Efficiency (NS)
  NS <- 1 - mean(err^2) / measvar
  
  # Insert efficiencies into function output
  eff <- c(merr, NS)
  eff_info <- c('mean residual', 'NS')
  
  # Display efficiencies
  if (display_eff) {
    cat('\n')
    for (i in seq_along(eff)) {
      cat(eff_info[i], ' = ', sprintf('%.2f', eff[i]), '\n')
    }
    cat('\n')
  }
  
  return(list(eff = eff, eff_info = eff_info))
}

# Example usage:
# obs <- c(1, 2, 3, 4, 5)
# mod <- c(0.9, 1.8, 3.2, 4.1, 5.2)
# result <- Eff_SAS(obs, mod)
# print(result)
