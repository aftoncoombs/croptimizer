## Tests filter_to_season()

test_that("filtering to each season works", {
  expect_equal(unique(filter_to_season(season = "fall", crop_data = crops)$growth_season_fall),
               TRUE)
})

test_that("providing season of length > 1 produces error", {
  expect_error(filter_to_season(season = "blah", data = crops))
})

test_that("providing season of class other than character produces error", {
  expect_error(filter_to_season(season = 1, data = crops))
})

test_that("providing season of class other than character produces error", {
  expect_error(filter_to_season(season = "fall", data = c(1,2,3)))
})

test_that("filtering to spring produces only crops that grow in spring", {
  spring_test <- filter_to_season(season = "spring")
  expect_equal(sum(spring_test$growth_season_spring == TRUE),
               nrow((spring_test)))
})

test_that("filtering to summer produces only crops that grow in summer", {
  summer_test <- filter_to_season(season = "summer")
  expect_equal(sum(summer_test$growth_season_summer == TRUE),
               nrow((summer_test)))
})

test_that("filtering to fall produces only crops that grow in fall", {
  fall_test <- filter_to_season(season = "fall")
  expect_equal(sum(fall_test$growth_season_fall == TRUE),
               nrow((fall_test)))
})

test_that("filtering to winter produces only crops that grow in winter", {
  winter_test <- filter_to_season(season = "winter")
  expect_equal(sum(winter_test$growth_season_winter == TRUE),
               nrow((winter_test)))
})

test_that("filtering to spring produces correct days remaining", {
  spring_test <- filter_to_season(season = "spring")
  expect_equal(sum(spring_test$total_days_in_growth_season ==
                     spring_test$days_remaining_in_growth_season),
               nrow((spring_test)))
})

test_that("filtering to summer produces correct days remaining", {
  summer_test <- filter_to_season(season = "summer")
  expect_equal(sum(summer_test$days_remaining_in_growth_season ==
                     (summer_test$total_days_in_growth_season -
                        28 * summer_test$growth_season_spring)),
               nrow(summer_test))
})

test_that("filtering to fall produces correct days remaining", {
  fall_test <- filter_to_season(season = "fall")
  expect_equal(sum(fall_test$days_remaining_in_growth_season ==
                     (fall_test$total_days_in_growth_season -
                        28 * fall_test$growth_season_summer -
                        28 * fall_test$growth_season_spring)),
               nrow(fall_test))
})
