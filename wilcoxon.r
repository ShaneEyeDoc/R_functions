# This function performs Wilcoxon rank-sum test for each dependent variable listed in a text file
wilcox <- function(dt, group_col, dv = NULL, 
                                   dv_file = "dependent_vars.txt", 
                                   alternative = "two.sided") {
  
  # Load dependent variables from file if not provided
  if (is.null(dv)) {
    dv <- scan(dv_file, what = character(), sep = "\n")
  }
  
  # Ensure dt is a data.table
  setDT(dt)
  
  # Check if grouping column exists
  if (!(group_col %in% colnames(dt))) {
    stop(paste("Grouping column", group_col, "not found in the data!"))
  }
  
  # Check if the grouping column has exactly 2 groups
  group_levels <- unique(dt[[group_col]])
  if (length(group_levels) != 2) {
    stop("The grouping column must have exactly **two** unique values for comparison!")
  }
  
  # Initialize results dataframe
  results <- data.frame(Dependent_Var = character(),
                        Group1 = character(),
                        Group2 = character(),
                        W_Statistic = numeric(),
                        P_Value = numeric(),
                        stringsAsFactors = FALSE)
  
  # Loop through each dependent variable
  for (dv_var in dv) {
    
    # Check if the dependent variable exists
    if (!(dv_var %in% colnames(dt))) {
      warning(paste("Skipping:", dv_var, "not found in data"))
      next
    }
    
    # Extract values for each group
    group1_data <- dt[get(group_col) == group_levels[1], ..dv_var][[1]]
    group2_data <- dt[get(group_col) == group_levels[2], ..dv_var][[1]]
    
    # Perform Wilcoxon rank-sum test
    test_result <- wilcox.test(group1_data, group2_data, alternative = alternative)
    
    # Store results (no filtering)
    results <- rbind(results, data.frame(Dependent_Var = dv_var,
                                         Group1 = group_levels[1],
                                         Group2 = group_levels[2],
                                         W_Statistic = round(test_result$statistic, 2),
                                         P_Value = round(test_result$p.value, 4)))
  }
  
  # Sort by p-value (smallest first)
  results <- results[order(results$P_Value), ]
  
  return(results)
}