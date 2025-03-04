# This function takes a data frame and returns a summary of categorical variables.
# It can either use specified columns or read column names from a text file
# # Option 1: Manually specify columns
# summary_cat <- sum_cat(dt, columns = c("gender", "status"))

# # Option 2: Read column names from a text file (one column name per line)
# summary_cat <- sum_cat(dt, file = "categorical_columns.txt")

sum_cat <- function(df, columns = NULL, file = NULL, bin_method = "quantile", bins = 4) {
  library(dplyr)
  library(tidyr)
  
  # 1. Use specified columns if provided
  if (!is.null(columns)) {
    cat_vars <- columns
  } 
  # 2. Read column names from a text file if provided (without readr)
  else if (!is.null(file)) {
    cat_vars <- scan(file, what = character(), quiet = TRUE)  # Read column names
  } 
  # 3. Auto-detect categorical and numeric columns
  else {
    cat_vars <- names(df)[sapply(df, function(x) is.character(x) | is.factor(x) | is.numeric(x))]  
  }
  
  # Ensure valid columns exist
  cat_vars <- intersect(cat_vars, names(df))
  
  if (length(cat_vars) == 0) {
    stop("No categorical columns found!")
  }
  
  # Convert numeric columns to categorical
  df <- df %>%
    mutate(across(all_of(cat_vars), ~ {
      if (is.numeric(.x)) {
        unique_vals <- unique(na.omit(.x))  # Get unique values, remove NA
        
        if (length(unique_vals) < bins) {
          # If too few unique values, treat as factor directly
          factor(.x)
        } else {
          # Bin into categories using quantiles or pretty breaks
          breaks <- unique(quantile(.x, probs = seq(0, 1, length.out = bins + 1), na.rm = TRUE))
          
          if (length(breaks) <= 2) {
            factor(.x)  # Fall back to treating as factor if breaks aren't unique
          } else {
            cut(.x, breaks = breaks, include.lowest = TRUE, labels = FALSE)
          }
        }
      } else {
        factor(.x)  # Ensure categorical variables are factors
      }
    }))
  
  # Summarize categorical data
  df %>%
    select(all_of(cat_vars)) %>%
    pivot_longer(cols = everything(), names_to = "Variable", values_to = "Value") %>%
    group_by(Variable, Value) %>%
    summarise(Count = n(), .groups = "drop") %>%
    mutate(Proportion = Count / sum(Count)) %>%
    arrange(Variable, desc(Count))  # Sort by Variable and descending count
}