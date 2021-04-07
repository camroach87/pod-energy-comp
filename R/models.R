#' Predict demand
#'
#' @param data (data frame) Data frame containing predictors and response variable `demand_mw`.
#' @param train_idx (integer) Vector of integers specifying which rows to use for training model
#' @param test_idx (integer) Vector of integers specifying which rows to use for testing model
#' @param ... Additional arguments passed to `lgb.fit`.
#'
#' @return
#' @export
pred_demand <- function(data,
                        train_idx,
                        test_idx,
                        ...) {
  
  response_idx <- which(colnames(data)=="demand_mw")
  pred_point(data, train_idx, test_idx, response_idx, ...)
}

#' Predict demand with models for each period
#'
#' @param data (data frame) Data frame containing predictors and response variable `demand_mw`. Must also contain `period` column.
#' @param train_idx (integer) Vector of integers specifying which rows to use for training model
#' @param test_idx (integer) Vector of integers specifying which rows to use for testing model
#' @param ... Additional arguments passed to `lgb.fit`.
#'
#' @return
#' @export
pred_demand_period <- function(data,
                               train_idx,
                               test_idx,
                               ...) {
  response_idx <- which(colnames(data)=="demand_mw")
  period_list <- unique(data$period)
  preds <- rep(NA, length(test_idx))
  for (i in period_list) {
    period_idx <- which(data$period == i)
    period_train_idx <- intersect(period_idx, train_idx)
    period_test_idx <- intersect(period_idx, test_idx)
    pred_idx <- period_test_idx - min(test_idx) + 1
    preds[pred_idx] <- pred_point(
      data[,-which(colnames(data)=="period")],  # remove as constant
      period_train_idx, 
      period_test_idx, 
      response_idx, 
      ...
    )
  }
  preds
}


#' Predict PV power
#'
#' @param data (data frame) Data frame containing predictors and response variable `pv_power_mw`.
#' @param train_idx (integer) Vector of integers specifying which rows to use for training model
#' @param test_idx (integer) Vector of integers specifying which rows to use for testing model
#' @param ... Additional arguments passed to `lgb.fit`.
#'
#' @return
#' @export
pred_pv <- function(data,
                    train_idx,
                    test_idx,
                    ...) {
  response_idx <- which(colnames(data)=="pv_power_mw")
  pv_pred <- pred_point(data, train_idx, test_idx, response_idx, ...)
  ifelse(pv_pred < 1e-2, 0, pv_pred)  # TODO: make 1e-2 a threshold argument 1e-2 MW is 100 W, which I think is reasonable to ignore
}


#' Predict PV power with quantile selection
#'
#' @param data (data frame) Data frame containing predictors, response variable `pv_power_mw` and `datetime`.
#' @param train_idx (integer) Vector of integers specifying which rows to use for training model
#' @param test_idx (integer) Vector of integers specifying which rows to use for testing model
#' @param alpha (numeric) Vector of quantiles 0.01-0.99.
#' @param ... Additional arguments passed to `lgb.fit`.
#'
#' @return
#' @export
#' 
#' @importFrom dplyr pull
#' @importFrom rlang .data
pred_pv_quantile <- function(data,
                             train_idx,
                             test_idx,
                             alpha = seq(0.5,0.9,0.01),
                             ...) {
  dots = list(...)
  datetimes <- select(data, .data$datetime)
  data <- select(data, -.data$datetime)
  response_idx <- which(colnames(data)=="pv_power_mw")
  pred_list <- map(
    set_names(alpha), 
    ~ pred_quantile(data, train_idx, test_idx, response_idx, alpha = .x, dots)
  )
  
  # Combine quantile predictions
  pv_pred <- bind_cols(datetimes[test_idx,], as_tibble(pred_list)) %>% 
    pivot_longer(cols = -.data$datetime, names_to = "quantile", 
                 values_to = "pv_power_mw") %>% 
    mutate(quantile = as.numeric(.data$quantile),
           date = date(.data$datetime))
  
  # Find quantile closest to 6 MWh for each day
  # FIXME: hard coded MW and MWh values here
  # FIXME: 0.5 quantile not chosen sometimes despite being > 6 MWh. Need to
  # check if 0.5 quantile is already > 6 MWh and then start looking through
  # higher quantiles.
  pv_pred_sum <- pv_pred %>% 
    group_by(.data$date, .data$quantile) %>% 
    summarise(pv_power_mwh = sum(.data$pv_power_mw)/2) %>%  # convert MW to MWh
    group_by(.data$date) %>% 
    filter(abs(.data$pv_power_mwh - 6) == min(abs(.data$pv_power_mwh - 6))) %>% 
    ungroup()
  
  pv_pred <- pv_pred %>% 
    inner_join(pv_pred_sum, by = c("date", "quantile")) %>% 
    pull(.data$pv_power_mw)
  
  ifelse(pv_pred < 1e-2, 0, pv_pred)  # TODO: make 1e-2 a threshold argument 1e-2 MW is 100 W, which I think is reasonable to ignore
}


#' Predict point prediction using lightgbm
#'
#' @param data 
#' @param train_idx 
#' @param test_idx 
#' @param response_idx 
#' @param ... 
#'
#' @importFrom lightgbm lgb.train lgb.Dataset
#' @importFrom stats predict
pred_point <- function(data, train_idx, test_idx, response_idx, ...) {
  data.train <- as.matrix(data[train_idx,])
  data.train_label <- data.train[, response_idx, drop = TRUE]
  data.train <- data.train[, -response_idx, drop = FALSE]
  
  data.test <- as.matrix(data[test_idx,])
  data.test_label <- data.test[, response_idx, drop = TRUE]
  data.test <- data.test[, -response_idx, drop = FALSE]
  
  lgb.fit <- lgb.train(
    data = lgb.Dataset(
      data = data.train,
      label = data.train_label
    ),
    verbose = 0,
    force_col_wise = TRUE,
    ...
  )
  
  predict(lgb.fit, data.test)
}


#' Predict quantile predictions using lightgbm
#'
#' @param data 
#' @param train_idx 
#' @param test_idx 
#' @param response_idx 
#' @param alpha
#' @param ... 
#'
#' @importFrom lightgbm lgb.train lgb.Dataset
#' @importFrom stats predict
pred_quantile <- function(data, train_idx, test_idx, response_idx, alpha, ...) {
  message(paste("Fitting quantile", alpha, "..."))
  
  # FIXME: this is a duplication of the above code. Convert to function.
  data.train <- as.matrix(data[train_idx,])
  data.train_label <- data.train[, response_idx, drop = TRUE]
  data.train <- data.train[, -response_idx, drop = FALSE]
  
  data.test <- as.matrix(data[test_idx,])
  data.test_label <- data.test[, response_idx, drop = TRUE]
  data.test <- data.test[, -response_idx, drop = FALSE]
  
  lgb.fit <- lgb.train(
    data = lgb.Dataset(
      data = data.train,
      label = data.train_label
    ),
    verbose = 0,
    force_col_wise = TRUE,
    obj = "quantile",
    eval = "quantile",
    alpha = alpha,
    ...
  )
  
  predict(lgb.fit, data.test)
}
