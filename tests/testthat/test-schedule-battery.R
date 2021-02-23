library(podEnergyComp)
library(tidyverse)
library(lubridate)

setwd("../..")

fcst_start_date <- ymd("2018-10-16")
demand.data <- load_demand_data("inst/extdata/pod_ds_task1")
demand.cv <- cv_ts_folds(demand.data$datetime,
                         start_date = fcst_start_date,
                         horizon = 7, 
                         iterations = 1)
demand.forecast <- pred_demand(demand.data,
                               demand.cv[[1]]$train, 
                               demand.cv[[1]]$test,
                               lambda_l1 = 1.0)
pv.data <- load_pv_data("inst/extdata/pod_ds_task1")
pv.cv <- cv_ts_folds(pv.data$datetime, 
                     start_date = fcst_start_date,
                     horizon = 7, 
                     iterations = 1)
pv.forecast <- pred_pv(pv.data,
                       pv.cv[[1]]$train,
                       pv.cv[[1]]$test, 
                       lambda_l1 = 1.5)
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
  expect_equivalent(C[,2:32], 0.5*t(apply(B, 1, cumsum))[,1:31])
  expect_gte(min(B, na.rm=T), -2.5)
  expect_lte(max(B, na.rm=T), 2.5)
  expect_lte(max(apply(t(B[,1:31]), 2, cumsum)), 12)
  expect_lte(max(C), 6)
  expect_equal(min(C), 0)
  expect_true(all(B[,32:42] <= 0))
  expect_true(all(B[,1:31] >= 0))
})

test_that("test battery data format", {
  agg_bat_df <- bat_df %>% 
    group_by(date = date(datetime)) %>% 
    mutate(cumsum_charge_MW = cumsum(charge_MW))
  
  expect_equal(max(agg_bat_df$cumsum_charge_MW), 12)
})

test_that("adjusted predicted profile is flat", {
  L_adj <- L + B
  L_adj <- L_adj[,32:42]
  # expect one unique value between periods 32 and 42 if flat
  for (i in rownames(L_adj)) {
    expect_equal(length(unique(L_adj[!!i,])), 1)
  }
})
