# # This function lets you create boxplots with jittered points for multiple y variables against a single x variable.
# # boxplot(dt, "guttae", y = c("min_epithickness_distance_od", "max_epithickness_distance_od"))
# boxplot <- function(df, x, y, save = FALSE, output_dir = "plots") {
#   # Convert y to a character vector (if it’s a single value)
#   y_vars <- as.character(y)
  
#   # Ensure the output directory exists if saving plots
#   if (save && !dir.exists(output_dir)) dir.create(output_dir)
  
#   # Loop through all y variables and generate plots
#   plots <- lapply(y_vars, function(y_var) {
#     p <- ggplot(df, aes(x = .data[[x]], y = .data[[y_var]], fill = factor(.data[[x]]))) +
#       geom_boxplot(alpha = 0.8, size = 1, color = "black", notch = FALSE, outlier.shape = NA) +  
#       geom_jitter(width = 0.1, height = 0, size = 1.5, alpha = 0.7, color = "black") +  
#       scale_fill_brewer(palette = "Dark2") +  
#       theme_classic(base_size = 16) +
#       theme(
#         legend.position = "top",
#         legend.title = element_text(face = "bold", size = 16),
#         legend.text = element_text(size = 14),  
#         plot.title = element_text(face = "bold", hjust = 0.5, size = 18),  
#         axis.text = element_text(color = "black", size = 14),
#         axis.title = element_text(face = "bold", size = 16),
#         axis.ticks.length = unit(0.3, "cm"),
#         axis.ticks = element_line(size = 0.8)
#       ) +
#       labs(title = paste(y_var, "vs", x))  # Add title for better readability

#     # Save the plot if needed
#     if (save) {
#       filename <- file.path(output_dir, paste0(x, "_vs_", y_var, ".png"))
#       ggsave(filename, p, width = 8, height = 6)
#     }

#     return(p)
#   })

#   # Return all plots as a list
#   return(plots)
# }

boxplot <- function(df, x, y, save = FALSE, output_dir = "plots") {
  # Convert y to a character vector (if it’s a single value)
  y_vars <- as.character(y)
  
  # Ensure the output directory exists if saving plots
  if (save && !dir.exists(output_dir)) dir.create(output_dir)
  
  # Loop through all y variables and generate plots
  plots <- lapply(y_vars, function(y_var) {
    p <- ggplot(df, aes(x = .data[[x]], y = .data[[y_var]], fill = factor(.data[[x]]))) +
      geom_boxplot(alpha = 0.8, size = 1, color = "black", notch = FALSE, outlier.shape = 16, outlier.size = 2) +  # Keep outliers, remove jitter
      scale_fill_brewer(palette = "Dark2") +  
      theme_classic(base_size = 16) +
      theme(
        legend.position = "top",
        legend.title = element_text(face = "bold", size = 16),
        legend.text = element_text(size = 14),  
        plot.title = element_text(face = "bold", hjust = 0.5, size = 18),  
        axis.text = element_text(color = "black", size = 14),
        axis.title = element_text(face = "bold", size = 16),
        axis.ticks.length = unit(0.3, "cm"),
        axis.ticks = element_line(size = 0.8)
      ) +
      labs(title = paste(y_var, "vs", x))  # Add title for better readability

    # Save the plot if needed
    if (save) {
      filename <- file.path(output_dir, paste0(x, "_vs_", y_var, ".png"))
      ggsave(filename, p, width = 8, height = 6)
    }

    return(p)
  })

  # Return all plots as a list
  return(plots)
}

# # This function lets you create scatter plots for multiple y variables against a single x variable.
# # scatterplot(dt, "guttae", y = c("min_epithickness_distance_od", "max_epithickness_distance_od"), color_by = 'severity')
scatterplot <- function(df, x, y, color_by = NULL, add_regression = TRUE, save = FALSE, output_dir = "plots") {
  # Convert y to a character vector (if it’s a single value)
  y_vars <- as.character(y)
  
  # Ensure the output directory exists if saving plots
  if (save && !dir.exists(output_dir)) dir.create(output_dir)
  
  # Loop through all y variables and generate scatter plots
  plots <- lapply(y_vars, function(y_var) {
    p <- ggplot(df, aes(x = .data[[x]], y = .data[[y_var]])) +
      geom_point(aes(fill = if (!is.null(color_by)) factor(.data[[color_by]]) else NULL),  
                 shape = 21,  # Shape 21 allows border & fill
                 size = 3,    # Adjust dot size
                 stroke = 0.8,  # Border thickness
                 color = "black",  # Border color
                 alpha = 0.7) +  # Transparency
      theme_classic(base_size = 16) +
      theme(
        legend.position = "top",
        legend.title = element_text(face = "bold", size = 16),
        legend.text = element_text(size = 14),  
        plot.title = element_text(face = "bold", hjust = 0.5, size = 18),  
        axis.text = element_text(color = "black", size = 14),
        axis.title = element_text(face = "bold", size = 16),
        axis.ticks.length = unit(0.3, "cm"),
        axis.ticks = element_line(size = 0.8)
      ) +
      labs(title = paste(y_var, "vs", x))
    
    # Apply fill color if a categorical variable is used
    if (!is.null(color_by)) {
      p <- p + scale_fill_brewer(palette = "Dark2", name = color_by)
    }

    # Add regression line if requested
    if (add_regression) {
      p <- p + geom_smooth(method = "lm", formula = y ~ x, color = "blue", fill = "lightblue", alpha = 0.3)
    }

    # Save the plot if needed
    if (save) {
      filename <- file.path(output_dir, paste0(x, "_vs_", y_var, "_scatter.png"))
      ggsave(filename, p, width = 8, height = 6)
    }

    return(p)
  })

  # Return all plots as a list
  return(plots)
}