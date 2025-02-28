sig_cor <- function(dt, iv = NULL, dv = NULL,
                    iv_file = "independent_vars.txt", 
                    dv_file = "dependent_vars.txt",
                    correlation_type = "spearman") {
  
  # Check if iv and dv are provided, else load from files
  if (is.null(iv)) {
    iv <- scan(iv_file, what = character(), sep = "\n")
  }
  
  if (is.null(dv)) {
    dv <- scan(dv_file, what = character(), sep = "\n")
  }
  
  # Ensure dt is a data.table
  setDT(dt)
  
  # Extract only selected columns from dt
  iv_data <- dt[, ..iv]
  dv_data <- dt[, ..dv]
  
  # Initialize an empty data frame to store results
  results <- data.frame(Independent_Var = character(),
                        Dependent_Var = character(),
                        Correlation = numeric(),
                        P_Value = numeric())
  
  # Loop through each independent variable and dependent variable
  for (iv_var in iv) {
    for (dv_var in dv) {
      
      # Calculate the correlation and p-value for each pair
      corr_result <- cor.test(dt[[iv_var]], dt[[dv_var]], method = correlation_type, exact = FALSE)
      
      # Store all results (no filtering by p-value)
      results <- rbind(results, data.frame(Independent_Var = iv_var,
                                           Dependent_Var = dv_var,
                                           Correlation = round(corr_result$estimate, 2),
                                           p_value = round(corr_result$p.value, 4)))
    }
  }
  
  # Return the result sorted by absolute correlation strength
  results <- results[order(-abs(results$Correlation)), ]
  
  return(results)
}