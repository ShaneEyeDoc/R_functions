# This function provides summary statistics for numeric variables in a data frame
sum_num <- function(df) {
  df %>%
    select(where(is.numeric)) %>%  # Select only numeric columns
    summarise(across(everything(), list(
      Min = ~ min(.x, na.rm = TRUE), 
      Q1 = ~ quantile(.x, 0.25, na.rm = TRUE),
      Median = ~ median(.x, na.rm = TRUE), 
      Q3 = ~ quantile(.x, 0.75, na.rm = TRUE),
      Max = ~ max(.x, na.rm = TRUE), 
      Mean = ~ mean(.x, na.rm = TRUE), 
      SD = ~ sd(.x, na.rm = TRUE)
    ), .names = "{.col}_{.fn}")) %>%  
    pivot_longer(cols = everything(), names_to = c("Variable", "Statistic"), 
                 names_pattern = "(.+)_(.+)") %>%  
    pivot_wider(names_from = Statistic, values_from = value)
}
