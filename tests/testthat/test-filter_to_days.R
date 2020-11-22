## Tests filter_to_days()

test_that("day is non-null", {
  expect_error(filter_to_days())
})

test_that("values less than 1 or greater than 28 produce an error", {
  expect_error(filter_to_days(day = 0))
  expect_error(filter_to_days(day = 29))
})

test_that("non-numeric values for day produce an error", {
  expect_error(filter_to_days(day = "tea"))
})

test_that("result contains correctly filtered values", {
  expect_equal(unique(filter_to_days(day = 25, crop_data = crops)$total_days_in_growth),
               4)
})
