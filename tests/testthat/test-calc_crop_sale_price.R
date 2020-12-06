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
