

test_that("non-speed-gro rows do not change total days in growth", {
  no_speed_gro <- append_soil_mod(soil_mods = "quality fertilizer")
  expect_equal(croptimizer::crops$total_days_in_growth,
               no_speed_gro$total_days_in_growth)
})

test_that("speed-gro rows update total days in growth by 10% decrease", {
  speed_gro <-
    append_soil_mod(soil_mods = "speed-gro") %>%
    update_total_days_in_growth()
  expect_equal(floor(croptimizer::crops$total_days_in_growth * 0.9),
               speed_gro$total_days_in_growth)
})

test_that("deluxe speed-gro rows update total days in growth by 25% decrease", {
  speed_gro <-
    append_soil_mod(soil_mods = "deluxe speed-gro") %>%
    update_total_days_in_growth()
  expect_equal(floor(croptimizer::crops$total_days_in_growth * 0.75),
               speed_gro$total_days_in_growth)
})

test_that("hyper speed-gro rows update total days in growth by 33% decrease", {
  speed_gro <-
    append_soil_mod(soil_mods = "hyper speed-gro") %>%
    update_total_days_in_growth()
  expect_equal(floor(croptimizer::crops$total_days_in_growth * 0.66),
               speed_gro$total_days_in_growth)
})

test_that("error if level_10_agri is not null or not of class logical", {
  expect_error(update_total_days_in_growth(crop_data = croptimizer::crops,
                                           level_10_agri = 1))
  expect_error(update_total_days_in_growth(crop_data = croptimizer::crops,
                                           level_10_agri = as.integer(1)))
  expect_error(update_total_days_in_growth(crop_data = croptimizer::crops,
                                           level_10_agri = "boba"))
})

test_that("if level_10_agri, then crops grow faster by 10%", {
  no_agri <- update_total_days_in_growth(crop_data = croptimizer::crops)
  yes_agri <- update_total_days_in_growth(crop_data = croptimizer::crops,
                                          level_10_agri = TRUE)
  expect_equal(ceiling(no_agri$total_days_in_growth * 0.9),
               yes_agri$total_days_in_growth)
})

