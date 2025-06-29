#' @title Rolling Mean Wind Direction
#' @description
#' Calculates the rolling mean of wind direction using the circular mean method.
#' @param df A data frame containing wind data
#' @param dir_col The name of the column containing wind direction in degrees.
#' @param speed_col The name of the column containing wind speed (optional).
#' @param k The window size for the rolling mean (default is 24).
#' @param align The alignment of the rolling mean. Options are "right", "left", or "center" 
#' (default is "right", this will calculate the mean wind direction for the last k time units).
#' @param fill The value to use for missing data (default is NA).
#' @param suffix A suffix to append to the new column name (default is "roll").
#' @return A data frame with the rolling mean wind direction added as a new column.

roll_mean_wind_dir <- function(df, dir_col, speed_col = NULL, k = c(24), align = "right", fill = NA, suffix = c("h")) {
  
  # Extract direction and optional speed
  wd <- df[[dir_col]]
  ws <- if (!is.null(speed_col)) df[[speed_col]] else rep(1, length(wd))  # Default to 1 if wind speed not provided
  
  # Convert to radians
  wd_rad <- wd * pi / 180
  
  # Compute wind components (note the minus sign for "coming from" convention)
  u <- -ws * sin(wd_rad)
  v <- -ws * cos(wd_rad)
  
  # Loop over each k to compute rolling mean wind direction
  count = 1
  for (i in k) {
    u_roll <- zoo::rollmean(u, k = i, align = align, fill = fill)
    v_roll <- zoo::rollmean(v, k = i, align = align, fill = fill)
    
    wd_roll <- (atan2(-u_roll, -v_roll) * 180 / pi) %% 360
    
    new_colname <- paste0(dir_col, "_", suffix[count])
    df[[new_colname]] <- wd_roll
    count = count + 1
  }
  
  return(df)
}



#' @title Classify Wind Direction
#' @description
#' Classifies wind direction in degrees into cardinal and intercardinal directions.
#' @param degrees numeric wind direction in degrees
#' @return A factor with levels "N", "NE", "E", "SE", "S", "SW", "W", "NW"

classify_wind_dir <- function(degrees) {
  dir_labels <- c("N", "NE", "E", "SE", "S", "SW", "W", "NW")
  breaks <- seq(-22.5, 360, by = 45)  # Note: covers wrap-around for N
  
  # Use cut() to assign class
  cut(
    degrees %% 360, 
    breaks = breaks, 
    labels = dir_labels, 
    include.lowest = TRUE, 
    right = FALSE
  )
}

# Same function as above but only for N and S
classify_wind_dir_NS <- function(degrees) {
  dir_labels <- c("N", "S")
  breaks <- c(-22.5, 157.5, 360)  # Note: covers wrap-around for N
  
  # Use cut() to assign class
  cut(
    degrees %% 360, 
    breaks = breaks, 
    labels = dir_labels, 
    include.lowest = TRUE, 
    right = FALSE
  )
}