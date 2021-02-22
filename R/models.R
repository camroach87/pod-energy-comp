pred_demand_bm <- function(data,
                           train_idx,
                           test_idx,
                           ...) {
  # TODO: use last week's actuals as forecast. Just subtract 7 days from test_idx dates to find demand.
}

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
  ifelse(pv_pred < 0, 0, pv_pred)
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
    num_leaves = 31L,
    # learning_rate = 1e-2,
    # nrounds = 1e3L,
    learning_rate = 0.1,
    nrounds = 100L,
    obj = "regression_l1",
    metric = "regression_l1",
    verbose = 0,
    force_row_wise = TRUE,
    ...
  )
  
  predict(lgb.fit, data.test)
}
