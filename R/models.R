pred_demand_bm <- function(data,
                           train_idx,
                           test_idx,
                           ...) {
  # TODO: use last week's actuals as forecast. Just subtract 7 days from test_idx dates to find demand.
}

#' Predict demand
#'
#' @param data 
#' @param train_idx 
#' @param test_idx 
#' @param ... 
#'
#' @return
#' @export
#'
#' @importFrom dplyr select filter
#' @importFrom tidyselect starts_with
#' @importFrom lightgbm lgb.Dataset lightgbm
pred_demand <- function(data,
                        train_idx,
                        test_idx,
                        ...) {
  data <- data %>% 
    select(demand_mw, period, yday, wday, starts_with("demand"),
           starts_with("temp"), starts_with("solar"))
  response_idx <- which(colnames(data)=="demand_mw")
  
  data.train <- data[train_idx,] %>% 
    filter(period %in% 32:42) %>% 
    as.matrix()
  data.train_label <- data.train[,response_idx]
  data.train <- data.train[,-response_idx, drop = FALSE]
  
  data.test <- data[test_idx,] %>% 
    filter(period %in% 32:42) %>% 
    as.matrix()
  data.test_label <- data.test[,response_idx]
  data.test <- data.test[,-response_idx, drop = FALSE]
  
  lgb.fit <- lgb.train(
    data = lgb.Dataset(
      data = data.train,
      label = data.train_label
    ),
    num_leaves = 31L,
    learning_rate = 1e-2,
    nrounds = 1e3L,
    obj = "regression",
    ...
  )
  predict(lgb.fit, data.test)
}

# TODO: same for PV
pred_pv <- function(data,
                    train_idx,
                    test_idx,
                    ...) {
}