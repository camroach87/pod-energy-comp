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
    select(datetime, sort(peek_vars()))
  
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
load_pv_data <- function() {
  pod %>% 
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
    select(-demand_mw) %>% 
    mutate(period = hh_to_period(datetime),
           month = month(datetime),
           yday = yday(datetime)) %>% 
    slice(-c(1:(48*7)))  # removes first 7 days missing week-lagged PV data
}

#' Load demand data
#'
#' @return Data frame containing demand (MW) data and required features for
#'   training demand model.
#' @export
#' 
#' @importFrom dplyr select mutate filter slice
#' @importFrom lubridate yday wday month ymd date
load_demand_data <- function() {
  pod %>% 
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
    select(-pv_power_mw) %>% 
    mutate(period = hh_to_period(datetime),
           month = month(datetime),
           yday = yday(datetime),
           wday = wday(datetime, week_start = 1)) %>%  # 1 = Monday
    slice(-c(1:(48*7))) %>%  # removes first 7 days missing week-lagged demand data
    filter(period %in% 32:42,  # train with charging periods only
           date(datetime) != ymd("2018-05-08"))  # outlier with 0 MW demand all day
}
