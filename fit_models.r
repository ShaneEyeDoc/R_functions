# function to fit multiple linear regression models with interaction and quadratic terms
fit_models <- function(dependent_vars, predictor_vars, data) {
    results <- list()
    
    # Loop through each dependent variable
    for (dep_var in dependent_vars) {
        formulas <- list()
        k <- 1
        
        # Generate formulas for each predictor
        for (var in predictor_vars) {
            formulas[[k]] <- as.formula(paste(dep_var, "~", var))
            k <- k + 1
            formulas[[k]] <- as.formula(paste(dep_var, "~", var, "+ I(", var, "^2)"))
            k <- k + 1
        }
        
        # If multiple predictors, add interaction and quadratic terms
        if (length(predictor_vars) > 1) {
            formulas[[k]] <- as.formula(paste(dep_var, "~", paste(predictor_vars, collapse = " + ")))
            k <- k + 1
            formulas[[k]] <- as.formula(paste(dep_var, "~", paste(predictor_vars, collapse = " * ")))
            k <- k + 1
            quadratic_terms <- paste(paste0("I(", predictor_vars, "^2)"), collapse = " + ")
            formulas[[k]] <- as.formula(paste(dep_var, "~", paste(predictor_vars, collapse = " + "), "+", quadratic_terms))
            k <- k + 1
            additional_interactions <- paste0("I(", predictor_vars[1], "^2) * ", predictor_vars[2], " + ", predictor_vars[1], 
                                              " * I(", predictor_vars[2], "^2) + ", "I(", predictor_vars[1], "^2) * I(", predictor_vars[2], "^2)")
            formulas[[k]] <- as.formula(paste(dep_var, "~", paste(predictor_vars, collapse = " + "), "+", quadratic_terms, 
                                              "+", additional_interactions))
            k <- k + 1
        }
        
        # Fit models for each dependent variable
        dep_results <- list()
        for (i in seq_along(formulas)) {
            model <- lm(formulas[[i]], data = data, na.action = na.exclude)
            model_name <- paste0(dep_var, "_m", i)
            dep_results[[model_name]] <- model  
            
            cat("\nSummary of", model_name, ":\n")
            print(summary(model))
            cat("\n")
        }
        
        results[[dep_var]] <- dep_results
    }
    
    return(results)
}

summarize_models <- function(models) {
    tidy_results <- list()
    glance_results <- list()
    
    # Loop through dependent variables and models
    for (dep_var in names(models)) {
        for (model_name in names(models[[dep_var]])) {
            model <- models[[dep_var]][[model_name]]
            tidy_results[[model_name]] <- tidy(model)
            glance_results[[model_name]] <- glance(model)
        }
    }
    
    tidy_df <- bind_rows(tidy_results, .id = "model")
    glance_df <- bind_rows(glance_results, .id = "model")
    
    model_stats_df <- glance_df %>% select(model, r.squared, adj.r.squared, statistic, p.value, df, AIC)
    coef_stats_df <- tidy_df %>% select(model, term, estimate, std.error, statistic, p.value)
    
    combined_df <- left_join(coef_stats_df, model_stats_df, by = "model")
    return(combined_df)
}
