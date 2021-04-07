#' Score forecasts
#'
#' Scores forecasts.
#'
#' @param file (chr) File containing forecasts in competition format.
#'
#' @importFrom readr read_csv cols col_double col_datetime
#' @importFrom dplyr select rename_all mutate filter group_by summarise
#'   inner_join case_when
#' @importFrom purrr set_names
#' @importFrom lubridate hour minute date
#' @importFrom stringr str_extract
#' @importFrom rlang .data
#'
#' @return Data frame containing peak reduction, PV proportion and scores.
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
    select(-.data$`_id`) %>% 
    rename_all(tolower) %>% 
    mutate(
      period = hh_to_period(.data$datetime),
      date = date(.data$datetime)
    )
  
  actual_df <- podEnergyComp::pod %>% 
    select(.data$datetime, .data$demand_mw, .data$pv_power_mw)
  
  charge_df <- charge_df %>% 
    inner_join(actual_df, by = "datetime") %>% 
    mutate(
      demand_adj_mw = .data$demand_mw + .data$charge_mw
    )
  
  # TODO: 1:31 and 32:42 should be global variables or maybe attributes for an
  # object?
  prop_df <- charge_df %>% 
    filter(.data$period %in% 1:31) %>% 
    mutate(p_dk = case_when(
      (.data$pv_power_mw > .data$charge_mw) & (.data$charge_mw != 0) ~ 
        .data$charge_mw, 
      .data$pv_power_mw < .data$charge_mw ~ .data$pv_power_mw,
      .data$charge_mw == 0 ~ 0
    )) %>% 
    group_by(.data$date) %>% 
    summarise(p_d1 = sum(.data$p_dk)/sum(.data$charge_mw),
              .groups = "drop") %>% 
    mutate(p_d2 = 1 - .data$p_d1)
  
  score_df <- charge_df %>% 
    filter(.data$period %in% 32:42) %>% 
    group_by(.data$date) %>% 
    summarise(peak_old_mw = max(.data$demand_mw),
              peak_new_mw = max(.data$demand_adj_mw),
              .groups = "drop") %>% 
    inner_join(prop_df, by = "date") %>% 
    mutate(r = 100*(.data$peak_old_mw - .data$peak_new_mw)/.data$peak_old_mw,
           s = .data$r*(3*.data$p_d1 + 1*.data$p_d2))
  
  score_df
}
