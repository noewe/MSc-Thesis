
create_interaction_formula <- function(target, spat_vars, met_vars) {

  # Construct formula with interactions
  interactions <- unlist(lapply(met_vars, function(met) {
    paste(met, spat_vars, sep = "*")
  }))
  full_formula <- as.formula(
    paste(target, "~", paste(c(spat_vars, met_vars, interactions), collapse = " + "))
  )
  
  message("Model formula:")
  print(full_formula)
  
  return(full_formula)
}



predict_model <- function(model, 
                          test_data, 
                          target, 
                          target_name, 
                          met_vars, 
                          spat_vars, 
                          model_type, 
                          model_type_short, 
                          plot_lim = c(0, 30)) {
  
  # Drop rows with NAs in test data
  predictor_vars <- unique(c(met_vars, spat_vars))
  n_before <- nrow(test_data)
  test_data <- test_data |> filter(if_all(all_of(predictor_vars), ~ !is.na(.)))
  n_after <- nrow(test_data)
  
  if (n_before != n_after) {
    message(sprintf("Dropped %d rows from test_data due to missing predictor values.", n_before - n_after))
  }
  
  # Predict on test data
  test_data <- test_data |>
    mutate(
      pred = predict(model, newdata = test_data),
      residuals = .data[[target]] - pred,
      rel_error = (pred - .data[[target]]) / .data[[target]]
    )
  
  # Per-logger metrics
  Err <- test_data |>
    group_by(Log_NR) |>
    summarise(
      RMSE_Log = sqrt(mean(residuals^2)),
      R2_Log = calc_rsquared(.data[[target]], pred),
      NRMSE_Log = RMSE_Log / mean(.data[[target]]),
      MBE_Log = mean(pred - .data[[target]]),
      .groups = "drop"
    )
  
  test_data <- left_join(test_data, Err, by = "Log_NR")
  
  # Overall metrics
  R2 <- round(calc_rsquared(test_data[[target]], test_data$pred), 3)
  RMSE <- round(sqrt(mean(test_data$residuals^2)), 3)
  MBE <- round(mean(test_data$pred - test_data[[target]]), 3)
  
  print(summary(model))
  
  # Plot predicted vs. actual
  plot <- ggplot(test_data, aes(x = pred, y = .data[[target]])) +
    geom_point(alpha = 0.2) +
    labs(
      x = paste(target_name, "predicted [K]"),
      y = paste(target_name, "measured [K]"),
      title = model_type,
      subtitle = substitute(
        R^2 == r2 ~ "; RMSE =" ~ rmse ~ "K" ~ "; MB =" ~ mbe ~ "K",
        list(r2 = R2, rmse = RMSE, mbe = MBE)
      )
    ) +
    geom_abline(intercept = 0, slope = 1, color = "red") +
    theme_bw()
  
  if (!is.na(plot_lim[1])) {
    plot <- plot + xlim(plot_lim) + ylim(plot_lim)
  }
  
  ggsave(paste0("../results/models/", model_type_short, "/mainplot_", model_type_short, ".png"),
         plot = plot, width=6.5, height=4, dpi=300, units = "in", limitsize = FALSE)
  
  print(plot)
  
  return(list(
    model = model,
    test_data = test_data,
    metrics = Err,
    plot = plot,
    R2 = R2,
    RMSE = RMSE,
    MBE = MBE
  ))
  
}

