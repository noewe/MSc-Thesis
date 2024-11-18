#--------------------------------------------------------------------------------
# Function for data download...
#--------------------------------------------------------------------------------

Logger_data <- function(city = "Bern",
                        date_start = "2024-05-15",
                        date_end = as.character(Sys.Date()),
                        write_csv = T,
                        interpolate = 0,
                        type = "temperature"){

#--------------------------------------------------------------------------------
# Load and run load.packages.R
#--------------------------------------------------------------------------------

  # Packages needed
  packages <- c("influxdbclient", "dplyr", "lubridate", "ggplot2", "tidyverse", "zoo", "leaflet")
  # Load the function
  suppressMessages(source('../R/load_packages.R'))
  # Load (and install if necessary) the packages
  suppressMessages(load_packages(packages))

#--------------------------------------------------------------------------------
# Choose a city
#--------------------------------------------------------------------------------

  # Guardian to make sure that the function does not crash if one use the wrong word
  # Interaction with the user to giv him advice how to choose the city correctly
  while (!tolower(city)%in%c("bern","thun", "biel")) {
    print("City is not defined correctly, please choose: Bern or Thun or Biel")
    city <- readline(prompt = "Enter your City: ")}

  # depending on your choice, the correct metadata sheet will be read
  if(tolower(city) == "bern"){
    meta <- read_csv('../Metadata/Bern/metadata_dynamic.csv', show_col_types = FALSE)
    print("Get data for Bern")}

  if(tolower(city) == "thun"){
    meta <- read_csv('../Metadata/Thun/metadata_Thun.csv', show_col_types = FALSE)
    print("Get data for Thun")}

  if(tolower(city) == "biel"){
    meta <- read_csv("../Metadata/Biel/Biel_metadata_static.csv", show_col_types = FALSE)
    print("Get data for Biel")}

  # Mutate the metadata sheet (proper start and end)
  meta <- meta |>
    mutate(End = as.Date(End, format = "%d.%m.%Y"),
           Start = as.Date(Start, format = "%d.%m.%Y"))|>
    mutate(End = if_else(is.na(End),Sys.Date(),End))

#--------------------------------------------------------------------------------
# Generate a proper file
#--------------------------------------------------------------------------------

#--------------------------------------------------------------------------------
# Download the data

  # token must be hidden at all times!
  token = "020TiviqUMHY9xL7Hbr63g_A5AozlXIKe93Ay9bdF7XAqAu1CjSejF4WSbo4JLdbzhsLFJzaJVECFqaqSeW3zg==" #token for access of data

  # Use the InfluxDBClient to get access to the logger data
  client <- InfluxDBClient$new(url = "https://influx.smcs.abilium.io",
                               token = token,
                               org = "abilium")

  # Create a date-sequence (from start to end in steps of 3 months for stability)
  date_seq <- seq.Date(from = as.Date(date_start, format = "%Y-%m-%d"), to = as.Date(date_end, format = "%Y-%m-%d"), by = "3 months")

  data_current <- list()
  # For-loop to download the data and assign it to the corresponding timestamp
  for (date in as.list(date_seq)) {
    print("Loading date range:")
    print(date)
    if(date+months(3)<as.Date(date_end)){date_end_local <- date+months(3)}else{date_end_local <- as.Date(date_end)}
    print(date_end_local)
    data_current[[length(data_current)+1]] <- client$query(paste0('from(bucket: "smcs") |> range(start: ', as.character(date), ', stop: ', date_end_local, ') |> filter(fn: (r) => r["_measurement"] == "mqtt_consumer") |> filter(fn: (r) => r["_field"] == "decoded_payload_temperature" or r["_field"] == "decoded_payload_humidity") |> filter(fn: (r) => r["topic"] != "v3/dynamicventilation@ttn/devices/eui-f613c9feff19276a/up") |> filter(fn: (r) => r["topic"] != "helium/eeea9617559b/rx") |> pivot(rowKey: ["_time"], columnKey: ["_field"], valueColumn: "_value")'))
  }

#--------------------------------------------------------------------------------
# Bind the data frames since it is better this way (tidy)

  tables <- bind_rows(data_current) |>
    # Change the format of the timestamo and add the QR code on the loggers (Code grafana)
    mutate(across(starts_with("_"), ~as.POSIXct(., format="%Y-%m-%dT%H:%M:%S%z")),
           Code_grafana = name)

#--------------------------------------------------------------------------------
# generate a proper table with the data

  # Many to many since several code grafanas per entry sometimes
  result <- inner_join(tables,meta, by = "Code_grafana",relationship = "many-to-many") |>
    # Now correct ones are assigned by date
    filter(date(time) >= Start & date(time) <= End) |>ungroup()|>
    # Round to 10minutes interval
    mutate(time = round_date(time, unit = "10 minutes")) |>
    # Group now to mean since some may have several
    group_by(time, Log_NR) |>
    # Now summarize
    summarize(temperature = mean(decoded_payload_temperature, na.rm = TRUE),
              humidity = mean(decoded_payload_humidity, na.rm = TRUE), .groups = "drop") |>
    # Important for order
    ungroup() |>
    # Now can be arranged
    arrange(Log_NR,time)

#--------------------------------------------------------------------------------
# Choose between temperature and relative humidity
#--------------------------------------------------------------------------------

  if(type == "temperature"){
    result <- result|> select(temperature,time,Log_NR)|>
      # Now make correct format in wide
      pivot_wider(names_from = Log_NR,
                  values_from = temperature,
                  id_cols = time)|>
      ungroup()|>
      arrange(time)|>
      rename_at(vars(-1), ~paste0("Log_", .))
  }else{result <- result|> select(humidity,time,Log_NR)|>
    # Now make correct format in wide
    pivot_wider(names_from = Log_NR,
                values_from = humidity,
                id_cols = time)|>
    ungroup()|>
    arrange(time)|>
    rename_at(vars(-1), ~paste0("Log_", .))
  }

#--------------------------------------------------------------------------------
# Load and rund interpolate.R if necessary
#--------------------------------------------------------------------------------

    if(interpolate > 0){
      source("../R/interpolate.R")
      result <- result |>
        mutate_all(~ fill_missing_temperatures(.,max_gap = interpolate))
    }

#--------------------------------------------------------------------------------
# Write a CSV if necessary
#--------------------------------------------------------------------------------

    if (write_csv) {
      # Define the folder path
      folder_path <- paste0("../data/", city, "/")

      # Check if the folder exists, create it if it doesn't
      if (!file.exists(folder_path)) {
        dir.create(folder_path, recursive = TRUE)
      }
      if(type == "temperature"){write_csv(result,paste0(folder_path, "Logger_data_T_",date_start,"_",date_end,".csv"))}else{
        write_csv(result,paste0(folder_path, "Logger_data_H_",date_start,"_",date_end,".csv"))
      }
    }

#--------------------------------------------------------------------------------
# Return the final file
#--------------------------------------------------------------------------------

    return(result)
}
