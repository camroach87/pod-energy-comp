test_that("scores correct", {
  expect_s3_class(sc, c("tbl_df", "tbl", "data.frame"))
  expect_equivalent(
    round(sc$s, 3),
    c(105.735, 104.859, 108.720, 106.085, 96.273, 108.708, 69.822)
  )
  expect_equal(sc$date, ymd("2018-07-23") + days(0:6))
  expect_true(all(0 <= sc$p_d1 & sc$p_d1 <= 1))
  expect_true(all(0 <= sc$p_d1 & sc$p_d1 <= 1))
})
