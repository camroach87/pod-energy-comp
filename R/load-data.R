#' Load data
#'
#' Loads demand, PV and weather data.
#'
#' @param path (chr) Path to data.
#' @param locations (int) Integers specifying which weather station locations to
#'   include. Possible values are 1-6.
#' @param inc_pv_cond (logical) If true, irradiance at PV facility
#'   (irradiance_wm2) and panel temperature (panel_temp_c) are also returned.
#' @param lags (list) List of lags to include for each variable variables.
#'   Element name should be variable and numeric values will give lags. Lags
#'   should be specified as number of half-hourly periods, e.g., c(1, 3, 5) will
#'   create 30, 90 and 150 minute lags.
#'
#' @return Data frame.
#' @export
#'
#' @importFrom readr read_csv cols col_datetime col_double
#' @importFrom dplyr select rename mutate bind_cols left_join full_join
#' @importFrom tidyselect peek_vars
#' @importFrom purrr map set_names
#' @importFrom lubridate ymd mdy
load_data <- function(path, locations = 1:6, inc_pv_cond = F,
                      lags = NULL) {
  # file names have different integers at end depending on batch release
  files <- list.files(path)
  files <- c("demand", "weather", "pv") %>% 
    set_names() %>% 
    map(~ files[grep(., files)])
  
  demand_df <- read_csv(
    file.path(path, files$demand),
    col_types = cols(
      datetime = col_datetime(),
      demand_MW = col_double()
    )
  ) %>% 
    rename(
      demand_mw = demand_MW
    )
  
  weather_df <- read_csv(
    file.path(path, files$weather),
    col_types = cols(
      datetime = col_datetime(),
      temp_location3 = col_double(),
      temp_location6 = col_double(),
      temp_location2 = col_double(),
      temp_location4 = col_double(),
      temp_location5 = col_double(),
      temp_location1 = col_double(),
      solar_location3 = col_double(),
      solar_location6 = col_double(),
      solar_location2 = col_double(),
      solar_location4 = col_double(),
      solar_location5 = col_double(),
      solar_location1 = col_double()
    )
  ) %>% 
    select(datetime, 
           matches(paste0("[", paste0(locations, collapse=""), "]{1}$")))
  
  pv_df <- read_csv(
    file.path(path, files$pv),
    col_types = cols(
      datetime = col_datetime(format = ""),
      `irradiance_Wm-2` = col_double(),
      pv_power_mw = col_double(),
      panel_temp_C = col_double()
    )
  ) %>% 
    rename(
      irradiance_wm2 = `irradiance_Wm-2`,
      panel_temp_c = panel_temp_C
    )
  
  if (!inc_pv_cond) pv_df <- select(pv_df, datetime, pv_power_mw)
  
  # interpolate hourly weather data to hh
  hh_dts <- seq(from=min(weather_df$datetime),
                to=max(weather_df$datetime) + 1800,
                by=1800)
  weather_df <- colnames(weather_df)[-1] %>% 
    set_names() %>% 
    map(~ approx(weather_df$datetime,
                 getElement(weather_df,.x),
                 hh_dts)$y) %>% 
    bind_cols() %>% 
    mutate(datetime = hh_dts)
  
  combine_df <- demand_df %>% 
    full_join(pv_df, by = "datetime") %>% 
    full_join(weather_df, by = "datetime") %>% 
    arrange(datetime)
  
  # Add lags
  lag_list <- list()
  if (!is.null(lags)) {
    for (i in seq_along(lags)) {
      lag_list[[i]] <- map(
        lags[[i]],
        ~ lag(getElement(combine_df, names(lags)[i]), n = .x)
      )
      names(lag_list[[i]]) <- paste0(names(lags)[i], "_lag", lags[[i]])
    }
  }
  
  # Tidy output
  combine_df <- combine_df %>% 
    bind_cols(lag_list) %>%                           # add lags
    filter(datetime >= min(demand_df$datetime)) %>%   # remove pre-demand data
    select(datetime, sort(peek_vars()))
  
  # Add public holidays
  pub_hol_df <- read_csv(
    file.path(path, "..", "England_Wales_public_holidays.csv"), 
    comment = "#",
    col_types = cols_only(
      Date = col_character()
    )
  ) %>% 
    rename(date = Date) %>% 
    mutate(date = mdy(date),
           public_holiday = 1)
  
  combine_df <- combine_df %>% 
    mutate(date = date(datetime)) %>% 
    left_join(pub_hol_df, by = "date") %>% 
    mutate(public_holiday = if_else(is.na(public_holiday), 0, public_holiday)) %>% 
    select(-date)
  
  combine_df
}


#' Load PV data
#'
#' @param path (chr) Path to data.
#'
#' @return Data frame containing PV generation (MW) data and required features
#'   for training PV generation model.
#' @export
#' 
#' @importFrom dplyr select mutate slice
#' @importFrom lubridate yday month
load_pv_data <- function(path) {
  load_data(
    path,
    locations = 1:6,
    lags = list(
      "pv_power_mw" = 48*7,
      "temp_location1" = 1:4,
      "temp_location2" = 1:4,
      "temp_location3" = 1:4,
      "temp_location4" = 1:4,
      "temp_location5" = 1:4,
      "temp_location6" = 1:4,
      "solar_location1" = 1:4,
      "solar_location2" = 1:4,
      "solar_location3" = 1:4,
      "solar_location4" = 1:4,
      "solar_location5" = 1:4,
      "solar_location6" = 1:4
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
#' @param path (chr) Path to data.
#'
#' @return Data frame containing demand (MW) data and required features for
#'   training demand model.
#' @export
#' 
#' @importFrom dplyr select mutate filter slice
#' @importFrom lubridate yday wday month
load_demand_data <- function(path) {
  load_data(
    path,
    locations = 1:6,
    lags = list(
      "demand_mw" = 48*7,
      "temp_location1" = c(2,6,12,24,48,96),
      "temp_location2" = c(2,6,12,24,48,96),
      "temp_location3" = c(2,6,12,24,48,96),
      "temp_location4" = c(2,6,12,24,48,96),
      "temp_location5" = c(2,6,12,24,48,96),
      "temp_location6" = c(2,6,12,24,48,96),
      "solar_location1" = c(2,6,12,24,48,96),
      "solar_location2" = c(2,6,12,24,48,96),
      "solar_location3" = c(2,6,12,24,48,96),
      "solar_location4" = c(2,6,12,24,48,96),
      "solar_location5" = c(2,6,12,24,48,96),
      "solar_location6" = c(2,6,12,24,48,96)
    )
  ) %>% 
    select(-pv_power_mw) %>% 
    mutate(period = hh_to_period(datetime),
           month = month(datetime),
           yday = yday(datetime),
           wday = wday(datetime, week_start = 1)) %>%  # 1 = Monday
    filter(period %in% 32:42,
           date(datetime) != ymd("2018-05-08")) %>%  # outlier with 0 MW demand all day
    slice(-c(1:(11*7)))  # removes first 7 days missing week-lagged demand data
}
