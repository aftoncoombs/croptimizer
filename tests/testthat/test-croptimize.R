test_that("croptimize works", {
  expect_silent(croptimize(season = "fall",
                           day_of_season = 1))
  expect_silent(croptimize(season = "fall",
                           day_of_season = 28))
  expect_silent(croptimize(season = "summer",
                           day_of_season = 20,
                           farming_level = 7,
                           level_5_tiller = TRUE))
})
