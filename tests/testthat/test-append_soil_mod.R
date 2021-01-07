test_that("when soil_mod is null, all soil mods are normal", {
  soil_mod_test <- append_soil_mod()
  expect_equal(sum(soil_mod_test$soil_mod == "normal"),
               nrow(soil_mod_test))
})

test_that("when soil_mod is non-named and wrong length, produce error",{
  expect_error(append_soil_mod(soil_mod = c("quality fertilizer")))
})

test_that("when soil_mod is non-names, vector is appended", {
  soil_mod_test <-
    append_soil_mod(soil_mods = rep("quality fertimilzer",
                                    nrow(croptimizer::crops)))
  expect_equal(soil_mod_test$soil_mod,
               rep("quality fertimilzer",
                   nrow(croptimizer::crops)))
})
