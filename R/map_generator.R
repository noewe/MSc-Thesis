# Function adapted from Nils Timmer and Patrick Bigler
# https://github.com/Urban-Climate-Unibe/Land_Canton_Bern

map_generator <- function(date, meteo_data, model, save = F){
  
  # Read tifs
  print('Reading geospatial data...')
  tiff_files <- list.files("../data/Geodata/LULCTopo_Thun/", pattern = "\\.tif$", full.names = TRUE) |>
    select(...) # select only the tiffs that are used in the model formula
  tiffs_only <- rast(tiff_files)
  
  print('Processing TIFF files...')
  
  if(is.na(date)){ #use example meteo data of existing night: 25.8.24, with an especially high UHI
    temp <- readRDS(...)
    tiffs_only <- c(tiffs_only, temp)
  } else {
    # Make rasters for each meteo variable of the selected date
    print(paste('Creating TIFFS layers of meteo data of', date, '...'))
    for (name_var in names(meteo_data)) {
      temp <- rast(ncol = 1200, nrow = 1630, #??
                   xmin = 2608999, xmax = 2619222, ymin = 1173148, ymax = 1183507, # Thun
                   names = name_var)
      values(temp) <- rep(meteo_data[name_var], ncell(temp))  # Fill with the value
      print(paste0(meteo_data[name_var], ": ", name_var))
      temp <- crop(temp, tiffs_only)
      temp <- resample(temp, tiffs_only)
      tiffs_only <- c(tiffs_only, temp)
    }
  }
    
  print('Model prediction in progress...')
  cores <- makeCluster(detectCores())
  temp_predict <- predict(tiffs_only, model, na.rm = TRUE, cores = cores)
  
  print('Model prediction successful. Creating a data frame...')
  temperature_df <- as.data.frame(temp_predict, xy = TRUE)
  
  # Check the prediction values
  print(summary(temperature_df$lyr1))  # Replace lyr1 with the actual name of the prediction
  
  # Generate the map
  print('Generating the map...')
  max_value <- max(abs(temperature_df$lyr1), na.rm = TRUE)
  
  p <- ggplot() +
    geom_raster(data = temperature_df, aes(x = x, y = y, fill = lyr1)) +
    scale_fill_gradient2(low = "blue4", mid = "white", high = "red4",
                         midpoint = 0, space = 'Lab', guide = 'colourbar',
                         aesthetics = 'fill', limits = c(-max_value, max_value)) +
    theme_classic() +
    theme(panel.border = element_rect(colour = "black", fill = NA, size = 1),
          title = element_text(size = 10, face = 'bold'),
          axis.title = element_text(size = 8),
          axis.text = element_text(size = 8),
          legend.title = element_text(size = 8),
          legend.text = element_text(size = 8),
          plot.subtitle = element_text(size = 8, face = 'plain'))
  
  if(save != F){
    pdf('analysis/Thunometer_lm.pdf', width = 10, height = 10)
    p
    dev.off()
  }
  
  print(p)
  print('Everything went fine!')
  return(p)
}


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
