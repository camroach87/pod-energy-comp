#' Add lags
#'
#' @param data (data frame) Data with variables to create lags for.
#' @param lags (list) List of lags to include for each variable variables.
#'   Element name should be variable and numeric values will give lags. Lags
#'   should be specified as number of half-hourly periods, e.g., c(1, 3, 5) will
#'   create 30, 90 and 150 minute lags.
#'
#' @return Data frame with additional lags
#' @export
#'
#' @importFrom dplyr bind_cols select
#' @importFrom tidyselect peek_vars
#' @importFrom purrr map
#' @importFrom rlang .data
add_lags <- function(data, lags = NULL) {
  lag_list <- list()
  if (!is.null(lags)) {
    for (i in seq_along(lags)) {
      lag_list[[i]] <- map(
        lags[[i]],
        ~ lag(getElement(data, names(lags)[i]), n = .x)
      )
      names(lag_list[[i]]) <- paste0(names(lags)[i], "_lag", lags[[i]])
    }
  }
  
  # Tidy output
  data <- data %>% 
    bind_cols(lag_list) %>% 
    select(.data$datetime, sort(peek_vars()))
  
  data
}


#' Adds extra features
#'
#' Minimum, maximum and mean temperatures are calculated across all weather
#' stations. Trend is a numeric variable increasing with date.
#'
#' @param data
#'
#' @return
#' @export
#'
#' @importFrom dplyr mutate select group_by summarise ungroup arrange inner_join lag
#' @importFrom tidyselect matches
#' @importFrom tidyr pivot_longer
#' @importFrom rlang .data
add_features <- function(data) {
  data <- data %>% 
    mutate(date = date(.data$datetime))
  
  # Calculate yesterday's max, min, mean temperatures
  temp_stats <- data %>% 
    select(.data$datetime, .data$date, matches("temp_location[1-6]{1}$")) %>% 
    pivot_longer(cols = -c(.data$datetime, .data$date), names_to = "location", 
                 values_to = "temp") %>% 
    group_by(.data$date) %>% 
    summarise(temp_min = min(.data$temp, na.rm=T), 
              temp_max = max(.data$temp, na.rm=T), 
              temp_mean = mean(.data$temp, na.rm=T)) %>% 
    ungroup() %>% 
    arrange(.data$date) %>% 
    mutate(temp_min_yday = lag(.data$temp_min, 1),
           temp_max_yday = lag(.data$temp_max, 1),
           temp_mean_yday = lag(.data$temp_mean, 1)) %>% 
    select(.data$date, .data$temp_min_yday, .data$temp_max_yday, 
           .data$temp_mean_yday)
  
  data <- data %>% 
    inner_join(temp_stats, by = "date")
  
  # Add trend
  data <- data %>% 
    mutate(trend = as.numeric(.data$date),
           trend = .data$trend - min(.data$trend) + 1) %>% 
    select(-.data$date)
  
  data
}


#' Load PV data
#'
#' @return Data frame containing PV generation (MW) data and required features
#'   for training PV generation model.
#' @export
#' 
#' @importFrom dplyr select mutate slice
#' @importFrom lubridate yday month
#' @importFrom rlang .data
load_pv_data <- function() {
  podEnergyComp::pod %>% 
    select(-.data$demand_mw) %>% 
    add_lags(
      lags = list(
        "pv_power_mw" = 48*7,
        "temp_location1" = 1:6,
        "temp_location2" = 1:6,
        "temp_location3" = 1:6,
        "temp_location4" = 1:6,
        "temp_location5" = 1:6,
        "temp_location6" = 1:6,
        "solar_location1" = 1:6,
        "solar_location2" = 1:6,
        "solar_location3" = 1:6,
        "solar_location4" = 1:6,
        "solar_location5" = 1:6,
        "solar_location6" = 1:6
      )
    ) %>% 
    mutate(period = hh_to_period(.data$datetime),
           month = month(.data$datetime),
           yday = yday_ly_adj(.data$datetime)) %>% 
    slice(-c(1:(48*7)))  # removes first 7 days missing week-lagged PV data
}

#' Load demand data
#'
#' @return Data frame containing demand (MW) data and required features for
#'   training demand model.
#' @export
#' 
#' @importFrom dplyr select mutate filter slice if_else between summarise ungroup
#' @importFrom lubridate yday wday month ymd date
#' @importFrom rlang .data
load_demand_data <- function() {
  podEnergyComp::pod %>% 
    select(-.data$pv_power_mw) %>% 
    add_lags(
      lags = list(
        "demand_mw" = 48*7,
        "temp_location1" = c(1,2,6,12,24,48,96),
        "temp_location2" = c(1,2,6,12,24,48,96),
        "temp_location3" = c(1,2,6,12,24,48,96),
        "temp_location4" = c(1,2,6,12,24,48,96),
        "temp_location5" = c(1,2,6,12,24,48,96),
        "temp_location6" = c(1,2,6,12,24,48,96),
        "solar_location1" = c(1,2,6,12,24,48,96),
        "solar_location2" = c(1,2,6,12,24,48,96),
        "solar_location3" = c(1,2,6,12,24,48,96),
        "solar_location4" = c(1,2,6,12,24,48,96),
        "solar_location5" = c(1,2,6,12,24,48,96),
        "solar_location6" = c(1,2,6,12,24,48,96)
      )
    ) %>% 
    add_features() %>% 
    mutate(
      # lockdown = if_else(between(date(datetime), ymd("2020-03-23"),
      #                            ymd("2020-06-23")), 1, 0),
      period = hh_to_period(.data$datetime),
      yday = yday_ly_adj(.data$datetime),
      wday = wday(.data$datetime, week_start = 1)  # 1 = Monday
    ) %>%  
    slice(-c(1:(48*7))) %>%  # removes first 7 days missing week-lagged demand data
    filter(
      .data$period %in% 32:42,  # FIXME: Hard coded. Train with charging periods only
      date(.data$datetime) != ymd("2018-05-08"),  # outlier 0 demand
      date(.data$datetime) != ymd("2018-05-10"),  # outlier high demand
      date(.data$datetime) != ymd("2018-11-04")   # outlier high demand
    )
}


#' Adjust yday for leap years
#' 
#' Adjusts yday values for leap years. 29 February is now assigned 59.5 and following dates are assigned their original yday minus 1. This ensures yday values are consistent with dates across all years.
#'
#' @param x (datetime) vector of datetime values.
#' 
#' @importFrom lubridate yday leap_year
yday_ly_adj <- function(x) {
  x_ly <- leap_year(x)
  x <- yday(x)
  x[x_ly & x == 60] <- 59.5                 # 29 Feb
  x[x_ly & x > 60] <- x[x_ly & x > 60] - 1  # > 29 Feb
  x
}
