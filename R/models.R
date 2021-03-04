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
  predict_lgbm(data, train_idx, test_idx, response_idx, ...)
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
    preds[pred_idx] <- predict_lgbm(
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
  pv_pred <- predict_lgbm(data, train_idx, test_idx, response_idx, ...)
  ifelse(pv_pred < 1e-2, 0, pv_pred)  # TODO: make 1e-2 a threshold argument 1e-2 MW is 100 W, which I think is reasonable to ignore
}


#' Predict using lightgbm
#'
#' @param data 
#' @param train_idx 
#' @param test_idx 
#' @param response_idx 
#' @param ... 
#'
#' @return
#'
#' @importFrom lightgbm lgb.train lgb.Dataset
predict_lgbm <- function(data, train_idx, test_idx, response_idx, ...) {
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
