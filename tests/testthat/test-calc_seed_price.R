## Runs tests for calc_seed_prices()

test_that("error when joja_member is not class logical", {
  expect_error(calc_seed_prices(joja_member = "boba"))
  expect_error(calc_seed_prices(joja_member = 42))
})

test_that("error when crop_data is not class data.frame", {
  expect_error(calc_seed_prices(crop_data = "boba"))
  expect_error(calc_seed_prices(crop_data = 42))
})
