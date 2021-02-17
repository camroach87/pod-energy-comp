library(dplyr)

test_that("load data", {
  setwd("../..")
  pod_data <- load_data("inst/extdata/pod_ds_task1/")
  # Test hourly weather values (in interpolated df) still match with raw data.
  # Test interpolated values are between raw hourly values.
  # Test max and min dates correct for each data drop. Probably need to create a table for this.
  
  expect_equal(pod_data, arrange(pod_data, datetime))   # test datetime order
  expect_true(all(!is.na(pod_data)))                    # test NA values
  
})
