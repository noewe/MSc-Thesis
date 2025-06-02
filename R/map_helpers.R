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



predict_temperature_map <- function(model,
                                    date,
                                    daynight = "n",
                                    meteo_dir = "../data/Geodata/Meteo_daily",
                                    lulc_dir = "../data/Geodata/LULCTopo_Thun",
                                    output_path = NULL, overwrite = TRUE) {

  # Extract predictor variable names (remove response)
  model_vars <- all.vars(formula(model))[-1]
  print(paste("Model variables:", paste(model_vars, collapse = ", ")))
  
  # Extract variable types from model
  model_terms <- terms(model)
  factor_vars <- attr(model_terms, "dataClasses")[attr(model_terms, "dataClasses") == "factor"]
  factor_vars <- names(factor_vars)
  
  raster_list <- list()
  missing_vars <- c()
  
  # for (var in model_vars) {
  #   # Construct raster paths
  #   meteo_path <- file.path(meteo_dir, date, daynight, paste0(var, ".tif"))
  #   lulc_path <- file.path(lulc_dir, paste0(var, ".tif"))
  #   
  #   if (file.exists(meteo_path)) {
  #     raster_list[[var]] <- rast(meteo_path)
  #   } else if (file.exists(lulc_path)) {
  #     raster_list[[var]] <- rast(lulc_path)
  #   } else {
  #     missing_vars <- c(missing_vars, var)
  #   }
  # }
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
    
    # If variable was a factor in the model, make raster a factor too
    if (var %in% factor_vars) {
      levels_in_model <- levels(model$model[[var]])
      r <- as.factor(r)
      levels(r) <- data.frame(value = as.numeric(levels_in_model), label = levels_in_model)
    }
    
    raster_list[[var]] <- r
  }
  
  if (length(missing_vars) > 0) {
    stop("Missing raster layers for predictors: ", paste(missing_vars, collapse = ", "))
  }
  
  print("All variables found. Proceeding with prediction...")
  
  # Stack and predict
  raster_stack <- rast(raster_list)
  prediction <- predict(raster_stack, model, na.rm = TRUE)
  print("Prediction successful!")
  
  # Save prediction raster
  if (!is.null(output_path)) {
    writeRaster(prediction, filename = paste0(output_path, "heatmap_", date, daynight, ".tif"), overwrite = overwrite)
  }
  
  # ---- Evaluate accuracy at measurement locations ----
  measurement_data <- Masterfile |> 
    filter(time == ymd(date)) |> 
    select(UHI, Log_NR, NORD_CHTOPO, OST_CHTOPO, LV_03_N, LV_03_E)
  
  # Convert to sf and extract prediction
  measurement_sf <- st_as_sf(measurement_data, coords = c("LV_03_E", "LV_03_N"), crs = crs(prediction))
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
  zlim <- range(c(df$predicted_temp, measurement_data$UHI), na.rm = TRUE)
  
  p <- ggplot(df, aes(x = x, y = y)) +
    geom_raster(aes(fill = predicted_temp)) +
    geom_point(
      data = measurement_data,
      aes(x = LV_03_E, y = LV_03_N, fill = UHI),
      color = "black",
      shape = 21,
      size = 3,
      stroke = 0.4
    ) +
    scale_fill_viridis_c(name = "UHI [K]", limits = zlim) +
    coord_equal(expand = FALSE) +
    labs(
      title = "Urban Heat Island, prediction and measurements",
      subtitle = paste(date, "|", ifelse(daynight == "d", "Day", "Night"), 
                       "\nR² =", round(r2, 3), "| RMSE =", round(rmse, 3), "K", "| MB =", round(mbe, 3), "K"),
      x = "Longitude", y = "Latitude"
    ) +
    theme_bw()
  
  print(p)
  # # Save plot
  if (!is.null(output_path)) {
    ggsave(filename = paste0(output_path, "heatmap_", date, daynight, ".png"), plot = p, width = 5, height = 5, dpi = 300)
  }
  
  # # Return result
  # return(list(
  #   raster = prediction,
  #   plot = p,
  #   metrics = list(R2 = r2, RMSE = rmse, MBE = mbe),
  #   measurement_data = measurement_data
  # ))
  
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
