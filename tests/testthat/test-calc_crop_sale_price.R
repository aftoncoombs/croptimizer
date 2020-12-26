test_that("error occurs when needed columns are missing", {
  expect_error(calc_crop_sale_price(crop_data = croptimizer::crops %>%
                                      dplyr::select(-sell_price)))

  expect_error(calc_crop_sale_price(crop_data = croptimizer::crops %>%
                                      dplyr::select(-chance_for_extra_crops)))

  expect_error(calc_crop_sale_price(crop_data = croptimizer::crops %>%
                                      dplyr::select(-max_extra_harvest)))
})

test_that("exp sale price = sell price + prob extra crops * num extra crops", {
  exp_value_test <- calc_crop_sale_price()
  expect_equal(exp_value_test$exp_sell_price,
               croptimizer::crops$sell_price +
                 (croptimizer::crops$chance_for_extra_crops *
                    croptimizer::crops$max_extra_harvest))
})

test_that("error when farming_level < 1", {
  expect_error(calc_crop_sale_price(farming_level = 0))
})

test_that("error when farming_level is not integer or numeric", {
  expect_error(calc_crop_sale_price(farming_level = "boba"))
  expect_error(calc_crop_sale_price(farming_level = TRUE))
})

test_that("error when max farm level exceeded", {
  expect_error(calc_crop_sale_price(farming_level = Inf, level_5_tiller = FALSE))
  expect_error(calc_crop_sale_price(farming_level = 14, level_5_tiller = FALSE))
})

test_that("error when farming_level >=5 and level_5_tiller not specified", {
  expect_error(calc_crop_sale_price(farming_level = 5))
})

test_that("error when level_5_tiller not of class logical", {
  expect_error(calc_crop_sale_price(farming_level = 5, level_5_tiller = "boba"))
  expect_error(calc_crop_sale_price(farming_level = 5, level_5_tiller = 1))
})

test_that("level_5_tiller results in 10% increase in crop sale price", {
  no_tiller <- calc_crop_sale_price()
  yes_tiller <- calc_crop_sale_price(level_5_tiller = TRUE)
  expect_equal(no_tiller$exp_sell_price * 1.1,
               yes_tiller$exp_sell_price)
})
