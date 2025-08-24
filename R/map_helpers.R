library(terra)
library(dplyr)
library(ggplot2)
library(sf)
library(lubridate)


create_meteo_tiffs <- function(date, 
                               meteo_path = "../data/final_predictors/Meteo_predictors_daily.rds", 
                               template_path = "../data/Geodata/LULCTopo_Thun/AD.tif", 
                               output_dir = "../data/Geodata/Meteo_daily/",
                               overwrite = FALSE) {
  
  
  # Load template raster
  template <- rast(template_path)
  
  # Load meteo data
  meteo_data <- readRDS(meteo_path)
  
  # Filter for specified date
  meteo_filtered <- meteo_data |> filter(time == ymd(date))
  if (nrow(meteo_filtered) == 0) stop("No data found for the specified date.")
  
  # Create output directories for day and night
  dir.create(file.path(output_dir, date, "d"), recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(output_dir, date, "n"), recursive = TRUE, showWarnings = FALSE)
  
  # Store rasters
  rasters <- list()
  
  for (dn in c("d", "n")) {
    subset <- meteo_filtered |> filter(daynight == dn) |> select(-time, -daynight)
    if (nrow(subset) != 1) warning(paste("Expected 1 row for", dn, "but found", nrow(subset)))
    
    for (var in names(subset)) {
      r <- template
      values(r) <- subset[[var]]
      key <- paste0(var, "_", dn)
      rasters[[key]] <- r
      
      # Save to file
      filename <- file.path(output_dir, date, dn, paste0(var, ".tif"))
      if (!file.exists(filename) || overwrite) {
        writeRaster(r, filename, overwrite = overwrite)
      }
    }
  }
  
  return(rasters)
}

library(terra)

predict_temperature_map <- function(model = NULL,
                                    date,
                                    daynight = "n",
                                    meteo_dir = "../data/Geodata/Meteo_daily",
                                    lulc_dir = "../data/Geodata/LULCTopo_Gauss",
                                    output_path = NULL, overwrite = TRUE,
                                    uhi_range = NULL) {
  if (is.null(model)) {
    print("Load model from folder.")
    model_path <- file.path(paste0(output_path, "model.rds"))
    if (!file.exists(model_path)) {
      stop("Model file does not exist at the specified path: ", model_path)
    }
    model <- readRDS(model_path)
  }
  
  # Extract predictor variable names (remove response)
  model_vars <- all.vars(formula(model))[-1]
  print(paste("Model variables:", paste(model_vars, collapse = ", ")))
  
  # Get factor variables
  model_terms <- terms(model)
  factor_vars <- names(attr(model_terms, "dataClasses")[attr(model_terms, "dataClasses") == "factor"])
  print(paste("Factor variables in model:", paste(factor_vars, collapse = ", ")))
  
  # Load rasters
  raster_list <- list()
  missing_vars <- c()
  
  for (var in model_vars) {
    meteo_path <- file.path(meteo_dir, date, daynight, paste0(var, ".tif"))
    lulc_path <- file.path(lulc_dir, paste0(var, ".tif"))
    
    if (file.exists(meteo_path)) {
      r <- rast(meteo_path)
    } else if (file.exists(lulc_path)) {
      r <- rast(lulc_path)
    } else {
      missing_vars <- c(missing_vars, var)
      next
    }
    
    raster_list[[var]] <- r
  }
  
  if (length(missing_vars) > 0) {
    stop("Missing raster layers for predictors: ", paste(missing_vars, collapse = ", "))
  }
  
  print("All variables found. Proceeding with prediction...")
  
  # Stack all predictors
  raster_stack <- rast(raster_list)
  
  # Convert to data frame
  raster_df <- as.data.frame(raster_stack, xy = FALSE, na.rm = FALSE)
  
  # Ensure factor levels match model training
  for (fvar in factor_vars) {
    if (fvar %in% names(raster_df)) {
      raster_df[[fvar]] <- factor(raster_df[[fvar]], levels = model$xlevels[[fvar]])
    }
  }
  
  # Predict
  predicted_values <- predict(model, newdata = raster_df)
  
  # Create output raster
  prediction_raster <- setValues(rast(raster_stack[[1]]), predicted_values)
  output_file <- paste0(output_path, "heatmap_", date, daynight, ".tif")
  
  writeRaster(prediction_raster, output_file, overwrite = overwrite)
  print("Prediction written to disk successfully!")
  
}


# Original function
# predict_temperature_map <- function(model = NULL,
#                                     date,
#                                     daynight = "n",
#                                     meteo_dir = "../data/Geodata/Meteo_daily",
#                                     lulc_dir = "../data/Geodata/LULCTopo_Gauss",
#                                     output_path = NULL, overwrite = TRUE,
#                                     uhi_range = NULL,
#                                     plot = FALSE) {
#   if(is.null(model)) {
#     print("Load model from folder.")
#     model_path <- file.path(paste0(output_path, "model.rds"))
#     if (!file.exists(model_path)) {
#       stop("Model file does not exist at the specified path: ", model_path)
#     }
#     model <- readRDS(model_path)
#   }
# 
#   # Extract predictor variable names (remove response)
#   model_vars <- all.vars(formula(model))[-1]
#   print(paste("Model variables:", paste(model_vars, collapse = ", ")))
# 
#   # Extract variable types from model
#   model_terms <- terms(model)
#   factor_vars <- attr(model_terms, "dataClasses")[attr(model_terms, "dataClasses") == "factor"]
#   factor_vars <- names(factor_vars)
#   print(paste("Factor variables in model:", paste(factor_vars, collapse = ", ")))
# 
#   raster_list <- list()
#   missing_vars <- c()
# 
#   for (var in model_vars) {
#     meteo_path <- file.path(meteo_dir, date, daynight, paste0(var, ".tif"))
#     lulc_path <- file.path(lulc_dir, paste0(var, ".tif"))
# 
#     if (file.exists(meteo_path)) {
#       r <- rast(meteo_path)
#     } else if (file.exists(lulc_path)) {
#       r <- rast(lulc_path)
#     } else {
#       missing_vars <- c(missing_vars, var)
#       next
#     }
# 
#     # If variable was a factor in the model, make raster a factor too
#     if (var %in% factor_vars) {
#       r <- as.factor(r)
# 
#       # Get only levels that actually appear in the model (non-NA coefficients)
#       model_coef_names <- names(coef(model))
#       used_levels <- model_coef_names[grepl(paste0("^", var), model_coef_names)]
#       used_levels_clean <- gsub(paste0(var), "", used_levels)
#       used_levels_clean <- gsub("^[:]", "", used_levels_clean)  # Remove colon if interaction term
#       used_levels_clean <- used_levels_clean[used_levels_clean != ""]  # Remove intercept etc.
# 
#       # Build levels table with integer codes
#       valid_levels <- unique(used_levels_clean)
#       valid_levels <- valid_levels[!is.na(valid_levels) & valid_levels != "(Intercept)"]
# 
#       # Make sure values are unique and match raster codes
#       level_df <- data.frame(value = seq_along(valid_levels), label = valid_levels)
# 
#       # Set factor levels
#       levels(r) <- level_df
#     }
# 
#     raster_list[[var]] <- r
#   }
# 
#   if (length(missing_vars) > 0) {
#     stop("Missing raster layers for predictors: ", paste(missing_vars, collapse = ", "))
#   }
# 
#   print("All variables found. Proceeding with prediction...")
# 
#   # Stack and predict directly to disk
#   raster_stack <- rast(raster_list)
#   output_file <- paste0(output_path, "heatmap_", date, daynight, ".tif")
# 
#   predict(raster_stack, model, na.rm = TRUE, filename = output_file, overwrite = overwrite)
# 
#   print("Prediction written to disk successfully!")
# 
#   # # Return result
#   # return(list(
#   #   raster = prediction,
#   #   plot = p,
#   #   metrics = list(R2 = r2, RMSE = rmse, MBE = mbe),
#   #   measurement_data = measurement_data
#   # ))
# 
# }

# remap_factor_raster <- function(r, var, model) {
#   # force to factor
#   r <- as.factor(r)
#   
#   # the levels your lm() actually saw:
#   model_lvls <- model$xlevels[[var]] %||% 
#     stop("No xlevels for ", var, " in the model")
#   
#   # grab the raster’s own level table
#   lvtbl <- levels(r)[[1]]
#   # terra usually calls them ID + category (or label)
#   if (!all(c("ID","category") %in% names(lvtbl))) {
#     stop("Unexpected level‐table columns: ", paste(names(lvtbl), collapse=", "))
#   }
#   
#   # keep only those rows whose category is in model_lvls
#   keep <- lvtbl$category %in% model_lvls
#   new_lvtbl <- lvtbl[keep, , drop=FALSE]
#   
#   if (nrow(new_lvtbl) < 2) {
#     stop("After filtering, <2 levels remain for ", var)
#   }
#   
#   # rename to exactly the two columns terra wants: value + label
#   new_lvtbl <- new_lvtbl[, c("ID","category")]
#   names(new_lvtbl) <- c("value","label")
#   
#   # assign back
#   levels(r) <- new_lvtbl
#   return(r)
# }
# 
# 
# predict_temperature_map <- function(model = NULL,
#                                     date,
#                                     daynight = "n",
#                                     meteo_dir = "../data/Geodata/Meteo_daily",
#                                     lulc_dir = "../data/Geodata/LULCTopo_Gauss",
#                                     output_path = NULL, overwrite = TRUE,
#                                     uhi_range = NULL,
#                                     plot = FALSE) {
#   if (is.null(model)) {
#     print("Load model from folder.")
#     model_path <- file.path(paste0(output_path, "model.rds"))
#     if (!file.exists(model_path)) {
#       stop("Model file does not exist at the specified path: ", model_path)
#     }
#     model <- readRDS(model_path)
#   }
#   
#   # Extract predictor variable names (remove response)
#   model_vars <- all.vars(formula(model))[-1]
#   print(paste("Model variables:", paste(model_vars, collapse = ", ")))
#   
#   # Extract variable types from model
#   model_terms <- terms(model)
#   factor_vars <- attr(model_terms, "dataClasses")[attr(model_terms, "dataClasses") == "factor"]
#   factor_vars <- names(factor_vars)
#   print(paste("Factor variables in model:", paste(factor_vars, collapse = ", ")))
#   
#   raster_list <- list()
#   missing_vars <- c()
#   
#   for (var in model_vars) {
#     meteo_path <- file.path(meteo_dir, date, daynight, paste0(var, ".tif"))
#     lulc_path <- file.path(lulc_dir, paste0(var, ".tif"))
#     
#     if (file.exists(meteo_path)) {
#       r <- rast(meteo_path)
#     } else if (file.exists(lulc_path)) {
#       r <- rast(lulc_path)
#     } else {
#       missing_vars <- c(missing_vars, var)
#       next
#     }
#     
#     if(var %in% factor_vars) {
#       r <- tryCatch(
#         remap_factor_raster(r, var, model),
#         error = function(e) {
#           stop("Failed to remap factor ‘", var, "’: ", e$message)
#         }
#       )
#     }
#     
#     raster_list[[var]] <- r
#   }
#   
#   if (length(missing_vars) > 0) {
#     stop("Missing raster layers for predictors: ", paste(missing_vars, collapse = ", "))
#   }
#   
#   print(paste("All variables found. Proceeding with prediction for", date, "..."))
#   
#   # Stack and predict directly to disk
#   raster_stack <- rast(raster_list)
#   output_file <- paste0(output_path, "heatmap_", date, daynight, ".tif")
#   
#   predict(raster_stack, model, na.rm = TRUE, filename = output_file, overwrite = overwrite)
#   
#   print("Prediction written to disk successfully!")
# }




plot_temperature_map <- function(model_type_short = NULL,
                                 dates = NULL,
                                 folder = "../results/models/",
                                 overwrite = TRUE,
                                 uhi_range = NULL, 
                                 daynight = "n",
                                 pal = NULL,
                                 xlim = c(7.575, 7.675), ylim = c(46.713, 46.792)) {

  # custom palette
  if(is.null(pal)) {
    pal <- c("#440154", "#443983", "#31688e", "#21918c", "#35b779", "#99d743", "#fdd725", "#FBA73AFF", "#ED7953FF","#D8576BFF", "#BD3786FF")
  }

  for(date in dates) {
  # load the tiff file in the folder
  prediction <-  terra::rast(paste0(folder, model_type_short, "/heatmap_", date, daynight, ".tif"))
  print(paste("Loaded raster for",  date))
  
  # crop raster by 500m on each side
  prediction <- terra::crop(prediction, terra::ext(prediction) + c(-1000, -1000, -500, -1000))
  
  # reproject to wgs84
  prediction <- terra::project(prediction, "EPSG:4326")
  
  # ---- Evaluate accuracy at measurement locations ----
  measurement_data <- Masterfile |> 
    filter(time == ymd(date)) |> 
    select(UHI, Log_NR, NORD_CHTOPO, OST_CHTOPO, LV_03_N, LV_03_E)
  
  # Convert to sf and extract prediction
  measurement_sf <- st_as_sf(measurement_data, coords = c("OST_CHTOPO", "NORD_CHTOPO"), crs = crs(prediction))
  measurement_data$pred <- terra::extract(prediction, vect(measurement_sf))[,2]
  
  # Compute metrics
  residuals <- measurement_data$pred - measurement_data$UHI
  rmse <- sqrt(mean(residuals^2, na.rm = TRUE))
  r2 <- 1 - sum(residuals^2, na.rm = TRUE) / sum((measurement_data$UHI - mean(measurement_data$UHI, na.rm = TRUE))^2)
  mbe <- mean(residuals, na.rm = TRUE)
  
  cat(sprintf("R² = %.3f ; RMSE = %.3f K ; MBE = %.3f K\n", r2, rmse, mbe))
  
  # ---- Create ggplot preview ----
  df <- as.data.frame(prediction, xy = TRUE, na.rm = TRUE)
  names(df)[3] <- "predicted_temp"
  
  # Combine predicted and measured values to get common limits
  print(paste("Range of predicted UHI values:", range(c(df$predicted_temp, measurement_data$UHI), na.rm = TRUE)))
  
  if(is.null(uhi_range)) {
    zlim <- range(c(df$predicted_temp, measurement_data$UHI), na.rm = TRUE)
    # abs(max(zlim)) # to get the same range for both positive and negative values
    zlim <- c(-abs(max(zlim)), abs(max(zlim)))
  } else{
    zlim <- uhi_range
  }
  
  p <- ggplot(df, aes(x = x, y = y)) +
    geom_raster(aes(fill = predicted_temp)) +
    geom_point(
      data = measurement_data,
      aes(x = OST_CHTOPO, y = NORD_CHTOPO, fill = UHI),
      color = "black",
      shape = 21,
      size = 3,
      stroke = 0.4
    ) +
    #scale_fill_viridis_c(name = "UHI [K]", limits = zlim) +
    # colorbrewer Spectral palette inverse, with midpoint at 0
    scale_fill_gradientn(name = "UHI [K]",
                         # colors  = rev(RColorBrewer::brewer.pal(9, "YlOrRd")),
                         # rainbow palette
                         colors = pal,
                         limits = zlim,
                         oob = scales::squish) +
    coord_sf(crs = "EPSG:4326", expand = FALSE,
             xlim = xlim, ylim = ylim) +
    labs(
      title = paste("UHI intensity — model:", model_type_short),
      subtitle = paste(date, "|", ifelse(daynight == "d", "Day", "Night"),
                       "\nR² =", round(r2, 3), "| RMSE =", round(rmse, 3), "K", "| MB =", round(mbe, 3), "K")
    ) +
    theme_minimal() +
    theme(
      legend.position = "right",
      axis.title = element_blank(),
      axis.text = element_blank(),
      plot.title = element_text(size = 13, face = "bold"),
      plot.subtitle = element_text(size = 11)
    )
  
  print(p)
  # # Save plot
  ggsave(filename = paste0(folder, model_type_short, "/heatmap3_", date, daynight, ".png"), plot = p, width = 5, height = 5, dpi = 600)
  
  }
}








# Function adapted from Nils Timmer and Patrick Bigler
# https://github.com/Urban-Climate-Unibe/Land_Canton_Bern

map_generator_leaflet <- function(meteo_data, model, save = F) {
  # Read tifs
  print('Reading geospatial data...')
  tiff_files <- list.files("../data/Tiffs/", pattern = "\\.tif$", full.names = TRUE)
  tiffs_only <- rast(tiff_files)
  
  print('Processing TIFF files...')
  # Process each variable in meteoswiss
  for (name_var in names(meteo_data)) {
    temp <- rast(ncol = 1200, nrow = 1630,
                 xmin = 2612000, xmax = 2618000, ymin = 1174000, ymax = 1182150,
                 names = name_var)
    values(temp) <- rep(meteo_data[name_var], ncell(temp))  # Fill with the value
    print(paste0(meteo_data[name_var], ": ", name_var))
    temp <- crop(temp, tiffs_only)
    temp <- resample(temp, tiffs_only)
    tiffs_only <- c(tiffs_only, temp)
  }
  
  print('TIFF processing successful. Model prediction in progress...')
  cores <- makeCluster(detectCores())
  temp_predict <- predict(tiffs_only, model, na.rm = TRUE, cores = cores)
  stopCluster(cores)
  
  print('Model prediction successful. Creating a data frame...')
  temperature_df <- as.data.frame(temp_predict, xy = TRUE)
  
  # Check the prediction values
  print(summary(temperature_df$lyr1))  # Replace lyr1 with the actual name of the prediction
  
  print('Generating the map...')
  max_value <- max(abs(temperature_df$lyr1), na.rm = TRUE)
  
  # Generate leaflet map
  pal <- colorNumeric(
    palette = c("blue4", "white", "red4"),
    domain = c(-max_value, max_value)
  )
  
  meta_final <- read_csv('data/meta_final.csv')
  
  leaflet_map <- leaflet() |>
    addProviderTiles("OpenStreetMap") |>
    addRasterImage(temp_predict, colors = pal, opacity = 0.8) %>%
    addLegend(pal = pal, values = temperature_df$lyr1, title = "Temperature") |>
    addCircleMarkers(
      data = meta_final,
      ~OST_CHTOPO, ~NORD_CHTOPO,
      label = ~Log_NR,
      radius = 5,
      color = ~ifelse(is.na(Code_grafana), "red", "green"),
      fill = TRUE,
      fillOpacity = 0.7)
  
  if (save != F) {
    saveWidget(leaflet_map, 'analysis/Thunometer_lm.html', selfcontained = TRUE)
  }
  print(leaflet_map)
  print('Everything went fine!')
  return(leaflet_map)
}
