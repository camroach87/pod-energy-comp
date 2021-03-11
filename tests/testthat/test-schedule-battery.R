library(podEnergyComp)
library(tidyverse)
library(lubridate)

fcst_start_date <- ymd("2019-12-18")
demand.data <- load_demand_data()
demand.cv <- cv_ts_folds(demand.data$datetime,
                         start_date = fcst_start_date,
                         horizon = 7, 
                         iterations = 1)
demand.forecast <- pred_demand(
  select(demand.data, -datetime),
  demand.cv[[1]]$train,
  demand.cv[[1]]$test,
  nrounds = 500L,
  num_leaves = 10L,
  learning_rate = 0.1,
  obj = "regression",
  metric = "regression"
)
pv.data <- load_pv_data()
pv.cv <- cv_ts_folds(pv.data$datetime, 
                     start_date = fcst_start_date,
                     horizon = 7, 
                     iterations = 1)
pv.forecast <- pred_pv_quantile(
  pv.data,
  pv.cv[[1]]$train,
  pv.cv[[1]]$test,
  alpha = seq(0.5,0.9),
  num_iterations = 250L,
  num_leaves = 31L,
  learning_rate = 0.03
)
demand.pred_df <- tibble(
  datetime = getElement(demand.data[demand.cv[[1]]$test,], "datetime"),
  demand_mw = demand.forecast
)
pv.pred_df <- tibble(
  datetime = getElement(pv.data[pv.cv[[1]]$test,], "datetime"),
  pv_power_mw = pv.forecast
)
fcst_df <- full_join(pv.pred_df, demand.pred_df, by = "datetime") %>% 
  mutate(period = 2*hour(datetime) + minute(datetime)/30 + 1)
b_sched <- schedule_battery(fcst_df)
bat_df <- format_charge_data(b_sched$B)
B <- b_sched$B
C <- b_sched$C
L <- b_sched$L

test_that("test battery schedule", {
  # check energy stored matches charges/discharges
  expect_equal(unname(C[,2:32]), 
               unname(0.5*t(apply(B, 1, cumsum))[,1:31]))
  expect_equal(dimnames(B), dimnames(C))
  for (i in rownames(B)) {
    # charge stored tests
    expect_true(all(C[!!i,1:10] == 0))
    expect_true(all(C[!!i,11:42] >= 0))
    expect_true(all(C[!!i,43:48] == 0))
    expect_equal(max(C[!!i,]), 6)
    expect_equal(min(C[!!i,]), 0)
    # charge/discharge tests
    expect_equal(max(cumsum(B[!!i,1:31])), 12)
    expect_equal(min(cumsum(B[!!i,32:42])), -12)
    expect_gte(min(B[!!i,], na.rm=T), -2.5)
    expect_lte(max(B[!!i,], na.rm=T), 2.5)
    expect_true(all(B[!!i,1:10] == 0))
    expect_true(all(B[!!i,11:31] >= 0))
    expect_true(all(B[!!i,32:42] <= 0))
    expect_true(all(B[!!i,43:48] == 0))
  }
})

test_that("test battery data format", {
  expect_equal(sum(is.na(bat_df)), 0)
  expect_equal(colnames(bat_df), c("_id", "datetime", "charge_MW"))
  expect_equal(dim(bat_df), c(336, 3))
  expect_equal(bat_df$`_id`, 1:336)
  
  # check battery capacity (6 MWh) is not exceeded
  agg_bat_df <- bat_df %>% 
    group_by(date = date(datetime)) %>% 
    mutate(cumsum_charge_MW = cumsum(charge_MW)) %>% 
    summarise(max_cumsum_charge_MW = max(cumsum_charge_MW),
              .groups = "drop")
  for (i in unique(agg_bat_df$date)) {
    expect_equal(agg_bat_df %>% 
                   filter(date == !!i) %>% 
                   pull(max_cumsum_charge_MW), 
                 12)
  }
  
  # check for small values
  expect_gt(min(abs(bat_df$charge_MW[bat_df$charge_MW != 0])),
            1e-6)
  
  # make sure all date times differ by 30 minutes and in correct order
  dts <- bat_df$datetime
  expect_equal(as.numeric(dts[-1] - dts[-length(dts)]), 
               rep(30, 335))
})

test_that("adjusted predicted profile is flat", {
  L_adj <- L + B
  L_adj <- L_adj[,32:42]
  # expect one unique value between periods 32 and 42 if flat
  for (i in rownames(L_adj)) {
    expect_equal(length(unique(L_adj[!!i,])), 1)
  }
})
