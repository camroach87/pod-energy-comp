test_that("scores correct", {
  setwd("../..")
  sc <- score_fcst_file("inst/extdata/pod_ds_task0/Cameron_set0.csv")
  expect_equivalent(
    round(sc, 3),
    c(105.735, 104.859, 108.720, 106.085, 96.273, 108.708, 69.822)
  )
})
