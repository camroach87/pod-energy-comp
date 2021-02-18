#' Get cross validation folds for time series
#'
#' @param x (datetime) Vector of dates to create indexes for. Must be ordered.
#' @param start_date (date) Date to start creating folds from.
#' @param horizon (int) Number of days making up each out-of-sample test set.
#' @param iterations (int) Number of training and test sets to create.
#'
#' @return List with train and test elements for each iteration.
#' @export
#' 
#' @importFrom lubridate days
#'
#' @examples
#' library(lubridate)
#' dts <- ymd("2020-01-01") + minutes(30)*0:(48*14-1)
#' cv_folds <- cv_ts_folds(dts, ymd("2020-01-07"), 2, 3)
cv_ts_folds <- function(x, start_date, horizon, iterations) {
  if (any(sort(x) != x)) stop("Datetime values must be ordered.")
  
  list_cv <- list()
  for (i in 1:iterations) {
    list_cv[[i]] <- list()
    list_cv[[i]][["train"]] <- which(x < start_date)
    list_cv[[i]][["test"]] <- which(x >= start_date & 
                                      x < start_date + days(horizon))
    start_date <- start_date + days(1)
  }
  
  list_cv
}