demand.data <- load_demand_data()

test_that("public holidays load", {
  # Test holidays
  pub_hol_vec <- ymd(c("2017-8-28", "2017-12-25", "2017-12-26", "2018-1-1", 
                       "2018-4-2"))
  pub_hol_vec <- demand.data %>% 
    filter(date(datetime) %in% pub_hol_vec) %>% 
    pull(public_holiday)
  expect_true(all(pub_hol_vec == 1))
  # Test non-holidays
  pub_hol_vec <- ymd(c("2017-8-27", "2017-12-12", "2017-12-24", "2018-1-2", 
                       "2018-4-3"))
  pub_hol_vec <- demand.data %>% 
    filter(date(datetime) %in% pub_hol_vec) %>% 
    pull(public_holiday)
  expect_true(all(pub_hol_vec == 0))
  
  pub_hol_yr <- demand.data %>% 
    filter(public_holiday == 1) %>% 
    dplyr::count(year = lubridate::year(datetime))
  expect_equal(pub_hol_yr$year, 2017:2020)
  expect_gt(min(pub_hol_yr$n), 0)
})

test_that("demand loads correctly", {
  demand.summ <- demand.data %>% 
    group_by(date = date(datetime)) %>% 
    summarise(demand_mw = sum(demand_mw),
              .groups = "drop")
  
  expect_equivalent(table(is.na(demand.summ))['TRUE'], 7)
  expect_gt(min(demand.summ$demand_mw, na.rm = T), 2)
  expect_lt(max(demand.summ$demand_mw, na.rm = T), 80)
})

test_that("leap years adjusted correctly", {
  expect_equal(max(demand.data$yday), 365)
  demand.data %>% 
    filter(between(date(datetime), ymd("2020-01-01"), ymd("2020-02-28"))) %>% 
    dplyr::distinct(yday) %>% 
    pull() %>% 
    expect_equal(1:59)
  demand.data %>% 
    filter(date(datetime) == ymd("2020-02-29")) %>% 
    dplyr::distinct(yday) %>% 
    pull() %>% 
    expect_equal(59.5)
  demand.data %>% 
    filter(date(datetime) > ymd("2020-02-29")) %>% 
    dplyr::distinct(yday) %>% 
    pull() %>% 
    expect_equal(60:190)
})