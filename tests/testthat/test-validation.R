library(lubridate)

test_that("cross validation works", {
  # check unordered dates
  dts <- ymd_hms("2020-01-01 00:00:00") + minutes(30)*c(1,3:400,2)
  expect_error(cv_ts_folds(dts, ymd("2020-01-02"), 1, 3),
               "Datetime values must be ordered.")
  
  # check two weeks of half-hourly data
  dts <- ymd("2020-01-01") + minutes(30)*0:(48*14-1)
  cv_folds <- cv_ts_folds(dts, ymd("2020-01-07"), 2, 3, 1)
  expect_equal(dts[cv_folds[[1]][["train"]]],
               ymd("2020-01-01") + minutes(30)*0:(48*6-1))
  expect_equal(dts[cv_folds[[1]][["test"]]],
               ymd("2020-01-07") + minutes(30)*0:(48*2-1))
  expect_equal(dts[cv_folds[[2]][["train"]]],
               ymd("2020-01-01") + minutes(30)*0:(48*7-1))
  expect_equal(dts[cv_folds[[2]][["test"]]],
               ymd("2020-01-08") + minutes(30)*0:(48*2-1))
  expect_equal(dts[cv_folds[[3]][["train"]]],
               ymd("2020-01-01") + minutes(30)*0:(48*8-1))
  expect_equal(dts[cv_folds[[3]][["test"]]],
               ymd("2020-01-09") + minutes(30)*0:(48*2-1))
  
  # check two weeks of half-hourly data with two day jumps
  dts <- ymd("2020-01-01") + minutes(30)*0:(48*14-1)
  cv_folds <- cv_ts_folds(dts, ymd("2020-01-07"), 2, 3, 2)
  expect_equal(dts[cv_folds[[1]][["train"]]],
               ymd("2020-01-01") + minutes(30)*0:(48*6-1))
  expect_equal(dts[cv_folds[[1]][["test"]]],
               ymd("2020-01-07") + minutes(30)*0:(48*2-1))
  expect_equal(dts[cv_folds[[2]][["train"]]],
               ymd("2020-01-01") + minutes(30)*0:(48*8-1))
  expect_equal(dts[cv_folds[[2]][["test"]]],
               ymd("2020-01-09") + minutes(30)*0:(48*2-1))
  expect_equal(dts[cv_folds[[3]][["train"]]],
               ymd("2020-01-01") + minutes(30)*0:(48*10-1))
  expect_equal(dts[cv_folds[[3]][["test"]]],
               ymd("2020-01-11") + minutes(30)*0:(48*2-1))
})
