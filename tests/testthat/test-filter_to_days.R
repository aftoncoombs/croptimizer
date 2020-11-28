## Tests filter_to_days()

test_that("day is non-null", {
  expect_error(filter_to_days())
  expect_error(filter_to_days(day_of_season = NULL))
})

test_that("values less than 1 or greater than 28 produce an error", {
  expect_error(filter_to_days(day = 0))
  expect_error(filter_to_days(day = 29))
})

test_that("non-numeric values for day produce an error", {
  expect_error(filter_to_days(day = "tea"))
})

test_that("result contains filtered values for crops the grow for one season", {
  expect_equal(unique(filter_to_days(day = 25,
                                     crop_data = crop_data[crop_data$total_days_in_growth_season == 28, ])$total_days_in_growth),
               4)
})

test_that("result contains filtered values for crops the grow for multiple season", {
  expect_equal(unique(filter_to_days(day = 25,
                                     crop_data = crop_data[crop_data$total_days_in_growth_season == 28, ])$total_days_in_growth),
               4)
})
