#' Score forecasts
#'
#' Scores forecasts.
#'
#' @param fcst (chr) File containing forecasts in competition format.
#' @param task (int) Forecast task number (0-5).
#'
#' @importFrom readr read_csv cols col_double col_datetime
#' @importFrom dplyr select rename_all mutate filter group_by summarise
#'   inner_join case_when
#' @importFrom purrr set_names
#' @importFrom lubridate hour minute date
#' @importFrom stringr str_extract
#'
#' @return Named vector with scores for each date.
#' @export
score_fcst_file <- function(file) {
  charge_df <- read_csv(
    file,
    col_types = cols(
      `_id` = col_double(),
      datetime = col_datetime(format = ""),
      charge_MW = col_double()
    )
  ) %>%
    select(-`_id`) %>% 
    rename_all(tolower) %>% 
    mutate(
      period = hour(datetime)*2 + minute(datetime)/30 + 1,
      date = date(datetime)
    )
  
  # Load actual data
  # FIXME: I'm going to have to figure out how to store data better. If this
  # package was installed, the inst/extdata directory wouldn't be available
  # anymore. So this is really quite a bizarre way to load the data.
  files <- list.files(file.path("inst", "extdata"))
  latest_task <- max(str_extract(files, "[0-6]{1}$"), na.rm = T)
  latest_data_path <- file.path("inst", "extdata", 
                                paste0("pod_ds_task", latest_task))
  actual_df <- load_data(latest_data_path) %>% 
    select(datetime, demand_mw, pv_power_mw)
  
  charge_df <- charge_df %>% 
    inner_join(actual_df, by = "datetime") %>% 
    mutate(
      demand_adj_mw = demand_mw + charge_mw
    )
  
  # TODO: 1:31 and 32:42 should be global variables or maybe attributes for an
  # object?
  prop_df <- charge_df %>% 
    filter(period %in% 1:31) %>% 
    mutate(p_dk = case_when(
      (pv_power_mw > charge_mw) & (charge_mw != 0) ~ charge_mw, 
      pv_power_mw < charge_mw ~ pv_power_mw,
      charge_mw == 0 ~ 0
    )) %>% 
    group_by(date) %>% 
    summarise(p_d1 = sum(p_dk)/sum(charge_mw),
              .groups = "drop") %>% 
    mutate(p_d2 = 1 - p_d1)
  
  score_df <- charge_df %>% 
    filter(period %in% 32:42) %>% 
    group_by(date) %>% 
    summarise(peak_old_mw = max(demand_mw),
              peak_new_mw = max(demand_adj_mw),
              .groups = "drop") %>% 
    inner_join(prop_df, by = "date") %>% 
    mutate(r = 100*(peak_old_mw - peak_new_mw)/peak_old_mw,
           s = r*(3*p_d1 + 1*p_d2))
  
  set_names(score_df$s, nm = score_df$date)
}
