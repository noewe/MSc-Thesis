#' Fits a linear regression model with interaction terms between meteorological and spatial variables.
#' The model and a simple plot of the results are saved to files.
#' @param data: A data frame containing the data to be used for modeling.
#' @param target: name of the target variable (dependent variable).
#' @param target_name: A string indicating the name of the target variable for plotting
#' @param met_vars: A vector of names of meteorological predictors
#' @param spat_vars: A vector of names of spatial predictors.
#' @param model_type: A string indicating the type of model (default is "LUR Model").
#' @param model_type_short: A short string for the model type (default is "lur_model").
#' @param algorithm: A string indicating the algorithm used (default is NA). "RF", "LUR"

#source("../R/model_helpers.R")

LUR_int_model <- function(train_data, test_data, target, target_name, met_vars, spat_vars, 
                          model_type = "LUR Model", model_type_short = "lur_model", 
                          save_model = TRUE, plot_lim = NA) {
  library(dplyr)
  library(ggplot2)
  library(mvrsquared)
  
  # Create results directory if needed
  dir.create(file.path("../results/models", model_type_short), recursive = TRUE, showWarnings = FALSE)
  
  # Drop NAs in training data
  train_data <- train_data |> filter(!is.na(.data[[target]]))
  test_data <- test_data |> filter(!is.na(.data[[target]]))
  
  full_formula <- create_interaction_formula(target, spat_vars, met_vars)
  
  # Fit model 
  model <- lm(full_formula, data = train_data)
  
  # pass on all arguments to the predicting function
  result <- predict_model(
    model = model,
    test_data = test_data,
    target = target,
    target_name = target_name,
    met_vars = met_vars,
    spat_vars = spat_vars,
    model_type = model_type,
    model_type_short = model_type_short,
    plot_lim = plot_lim
  )
  
  # Save model
  if (save_model) {
    saveRDS(model, file = paste0("../results/models/", model_type_short, "/model.rds"))
    saveRDS(result, file = paste0("../results/models/", model_type_short, "/result_list.rds"))
  }
  
  return(result)
}


      