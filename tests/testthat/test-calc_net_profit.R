test_that("error when sort is not of class logical", {
  pre_net_profit_data <-
    croptimizer::crops %>%
    calc_seed_prices(joja_member = FALSE, drop_cols = TRUE) %>%
    append_soil_mod(soil_mods = "Quality Fertilizer") %>%
    filter_to_season(season = "spring") %>%
    filter_to_days(day_of_season = 1) %>%
    calc_crop_sale_price(farming_level = 5, level_5_tiller = TRUE)

  expect_error(calc_net_profit(pre_net_profit_data, sort = "boba"))
})

test_that("error when crop_data does not contain column seed_price", {
  pre_net_profit_data <-
    croptimizer::crops %>%
    calc_seed_prices(joja_member = FALSE, drop_cols = TRUE) %>%
    append_soil_mod(soil_mods = "Quality Fertilizer") %>%
    filter_to_season(season = "spring") %>%
    filter_to_days(day_of_season = 1) %>%
    calc_crop_sale_price(farming_level = 5, level_5_tiller = TRUE)

  expect_error(calc_net_profit(pre_net_profit_data %>%
                                 dplyr::select(-seed_price)))
})

test_that("error when crop_data does not contain column exp_sell_price", {
  pre_net_profit_data <-
    croptimizer::crops %>%
    calc_seed_prices(joja_member = FALSE, drop_cols = TRUE) %>%
    append_soil_mod(soil_mods = "Quality Fertilizer") %>%
    filter_to_season(season = "spring") %>%
    filter_to_days(day_of_season = 1) %>%
    calc_crop_sale_price(farming_level = 5, level_5_tiller = TRUE)

  expect_error(calc_net_profit(pre_net_profit_data %>%
                                 dplyr::select(-exp_sell_price)))
})

test_that("net_profit has the correct value", {
  pre_net_profit_data <-
    croptimizer::crops %>%
    calc_seed_prices(joja_member = FALSE, drop_cols = TRUE) %>%
    append_soil_mod(soil_mods = "Quality Fertilizer") %>%
    filter_to_season(season = "spring") %>%
    filter_to_days(day_of_season = 1) %>%
    calc_crop_sale_price(farming_level = 5, level_5_tiller = TRUE)

  expect_equal(calc_net_profit(pre_net_profit_data, sort = FALSE)$net_profit,
               (pre_net_profit_data$exp_sell_price -
                  pre_net_profit_data$seed_price))
})

test_that("when sort == TRUE, results are sorted", {
  net_profit_data <-
    croptimizer::crops %>%
    calc_seed_prices(joja_member = FALSE, drop_cols = TRUE) %>%
    append_soil_mod(soil_mods = "Quality Fertilizer") %>%
    filter_to_season(season = "spring") %>%
    filter_to_days(day_of_season = 1) %>%
    calc_crop_sale_price(farming_level = 5, level_5_tiller = TRUE) %>%
    calc_net_profit(sort = TRUE)

  expect_equal(net_profit_data$net_profit,
               sort(net_profit_data$net_profit,
                    decreasing = TRUE,
                    na.last = TRUE))
})

