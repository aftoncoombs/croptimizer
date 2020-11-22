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
