
quality_control <- function(city = 'Bern'){

#--------------------------------------------------------------------------------
# Choose a city
#--------------------------------------------------------------------------------

  if(city =="Bern"){
    metadata <- read_csv("../Metadata/Bern/metadata_dynamic.csv", show_col_types = FALSE) |>
      mutate(Log_NR = paste0("Log_", Log_NR))
  }

  if(city == "Thun"){
    metadata <- read_csv("../Metadata/Thun/metadata_Thun.csv") |>
      mutate(Log_NR = paste0("Log_", Log_NR))
  }

  if(city == "Biel"){
    metadata <- read_csv("../Metadata/Biel/Biel_metadata_static.csv") |>
      mutate(Log_NR = paste0("Log_", Log_NR))
  }

#--------------------------------------------------------------------------------
# Table which provides an overview about all missing values
#--------------------------------------------------------------------------------

  data_missing <- data_cleaned |>
    #summarize by counting how many rows contain NA and dividing by number of rows
    summarize_all(~sum(is.na(.))/n()) |>
    # Add column names as row
    rbind(colnames(data_cleaned)) |>
    t() |>
    as.data.frame() |>
    rename(missing = V1,
           Log_NR = V2) |>
    mutate(missing = as.numeric(missing) * 100)

#--------------------------------------------------------------------------------
# Map which gives an overview about where the missing vaklues are
#--------------------------------------------------------------------------------

  lat_columns <- c("Lat", "Latitude", "NORD_CHTOPO", "NORD_CH_TOPO", "NORD_CHTOP")
  lon_columns <- c("Lon", "Longitude", "OST_CHTOPO", "OST_CH_TOPO", 'OST_CHTOP')

  metadata <- metadata |>
    left_join(data_missing, by = c("Log_NR"))|>
    rename(Latitude = any_of(lat_columns),
           Longitude = any_of(lon_columns))

  pal <- leaflet::colorNumeric("plasma", domain = metadata$missing)

  map <- leaflet::leaflet(metadata) |>
    leaflet::addTiles() |>
    leaflet::addCircleMarkers(~Longitude, ~Latitude,
                              fillColor = ~pal(missing), radius = 6, fillOpacity = 1,
                              color = "black", weight = 1, opacity = 1,
                              popup = paste(
                                "<strong>Measurement type: </strong>", round(metadata$missing))) |>
    leaflet::addLegend("bottomright", pal = leaflet::colorNumeric("plasma", domain = metadata$missing), values = metadata$missing, title = "Missing data in %")

#--------------------------------------------------------------------------------
# Return a list
#--------------------------------------------------------------------------------
  lst <- list(data_missing, map)
  return(lst)
}



