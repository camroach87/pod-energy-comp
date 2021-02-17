#' Load data
#'
#' Loads demand, PV and weather data.
#'
#' @param path (chr) Path to data.
#' @param locations (int) Integers specifying which weather station locations to
#'   include. Possible values are 1-6.
#' @param inc_pv_cond (logical) If true, irradiance at PV facility
#'   (irradiance_wm2) and panel temperature (panel_temp_c) are also returned.
#'
#' @return Data frame containing all joined data.
#' @export
#'
#' @importFrom readr read_csv cols col_datetime col_double 
#' @importFrom dplyr select rename mutate bind_cols left_join full_join
#' @importFrom purrr map set_names
#' @importFrom lubridate ymd
load_data <- function(path, locations = 1:6, inc_pv_cond = F) {
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
    left_join(weather_df, by = "datetime")
  
  combine_df
}
