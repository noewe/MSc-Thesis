#' Fits a linear regression model with interaction terms between meteorological and spatial variables.
#' The model and a simple plot of the results are saved to files.
#' @param data: A data frame containing the data to be used for modeling.
#' @param target: name of the target variable (dependent variable).
#' @param target_name: A string indicating the name of the target variable for plotting
#' @param met_vars: A vector of names of meteorological predictors
#' @param spat_vars: A vector of names of spatial predictors.
#' @param model_type: A string indicating the type of model (default is "LUR Model").
#' @param model_type_short: A short string for the model type (default is "lur_model").


LUR_int_model <- function(data, target, target_name, met_vars, spat_vars, model_type = "LUR Model", model_type_short = "lur_model", save_model = F, plot_lim = NA) {
  library(dplyr)
  library(ggplot2)
  library(mvrsquared)
  
  # create directory for models if it doesn't exist
  if (!dir.exists(paste0("../models/", model_type_short))) {
    dir.create(paste0("../models/", model_type_short))
  }
  
  # Construct the interaction formula
  interactions <- unlist(lapply(met_vars, function(met) {
    paste(met, spat_vars, sep = "*")
  }))
  
  full_formula <- as.formula(
    paste(target, "~",
          paste(c(spat_vars, met_vars, interactions), collapse = " + "))
  )
  
  print("Model formula:")
  print(full_formula)
  
  # Fit model and save it
  model <- lm(full_formula, data = data)
  if (save_model) {
    saveRDS(model, file = paste0("../models/", model_type_short, "_model.rds"))
  }
  
  # Predict and calculate residuals
  data <- data %>%
    mutate(
      pred = predict(model),
      residuals = .data[[target]] - pred,
      rel_error = (pred - .data[[target]]) / .data[[target]]
    )
  
  # Error metrics by logger
  Err <- data %>%
    group_by(Log_NR) %>%
    summarise(
      RMSE_Log = sqrt(mean(residuals^2)),
      R2_Log = calc_rsquared(.data[[target]], pred),
      NRMSE_Log = RMSE_Log / mean(.data[[target]])
    )
  
  data <- left_join(data, Err, by = "Log_NR")
  
  # Overall metrics
  R2 <- round(summary(model)$r.squared, 3)
  RMSE <- round(sqrt(mean(data$residuals^2)), 3)
  
  # Print model summary
  print(summary(model))
  
  # Plot
  plot <- ggplot(data, aes(x = pred, y = .data[[target]])) +
    geom_point(alpha = 0.2) +
    labs(
      x = paste(target_name, "predicted [K]"), 
      y = paste(target_name, "measured [K]"), 
      title = model_type,
      subtitle = substitute(R^2 == r2 ~ "; RMSE =" ~ rmse ~ "K", list(r2 = R2, rmse = RMSE))
    ) +
    geom_abline(intercept = 0, slope = 1, color = "red") +
    theme_bw()
    if (!missing(plot_lim)) {
      plot <- plot +
        xlim(plot_lim) + ylim(plot_lim)
    }
  
  print(plot)
  ggsave(paste0("../figures/models/", model_type_short, ".png"), plot = plot, width = 6.5, height = 4, dpi = 300)
  
  return(list(model = model, data = data, metrics = Err, plot = plot))
}
      