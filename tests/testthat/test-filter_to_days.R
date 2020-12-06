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


test_that("days in growth <= days remaining in growth season", {
  fall_test <-
    filter_to_season(season = "fall") %>% filter_to_days(day_of_season = 25)
  summer_test <-
    filter_to_season(season = "summer") %>% filter_to_days(day_of_season = 25)
  spring_test <-
    filter_to_season(season = "spring") %>% filter_to_days(day_of_season = 25)
  expect_equal(sum(fall_test$total_days_in_growth_season == Inf |
                     fall_test$total_days_in_growth <= 4),
               nrow(fall_test))
  expect_equal(sum(summer_test$days_remaining_in_growth_season >=
                     summer_test$total_days_in_growth),
               nrow(summer_test))
  expect_equal(sum(spring_test$days_remaining_in_growth_season >=
                     spring_test$total_days_in_growth),
               nrow(spring_test))
})
