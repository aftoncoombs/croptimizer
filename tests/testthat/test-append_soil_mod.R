test_that("when soil_mod is non-named and wrong length, produce error",{
  expect_error(append_soil_mod(soil_mod = c("quality fertilizer", "quality fertilizer")))
})

test_that("fertilizer names are standardized if possible", {
  expect_silent(append_soil_mod(soil_mods = "quality fertilizer"))
  expect_silent(append_soil_mod(soil_mods = "DELUXE FERTILIZER"))
})

test_that("error if non-standard fertilizer names", {
  ## Test if singleton
  expect_error(append_soil_mod(soil_mods = "Quality Boba"))

  ## Test if non-named vector of appropriate length
  expect_error(append_soil_mod(soil_mods = rep("Deluxe Boba", nrow(crops))))
  expect_error(append_soil_mod(soil_mods = c("Super Deluxe Boba", rep("Deluxe Boba", nrow(crops) - 1))))

  ## Test if a named vector of any length
  expect_error(append_soil_mod(soil_mods = c("Blueberry" = "Boba",
                                             "Pumpkin" = "Tea",
                                             "Bok Choy" = "Cookies & Candy")))
})

test_that("when soil_mod is null, all soil mods are Normal", {
  soil_mod_test <- append_soil_mod()
  expect_equal(sum(soil_mod_test$soil_mod == "Normal"),
               nrow(soil_mod_test))
})

test_that("when soil_mod is non-named, vector is appended", {
  soil_mod_test <-
    append_soil_mod(soil_mods = rep("quality fertilizer",
                                    nrow(croptimizer::crops)))
  expect_equal(soil_mod_test$soil_mod,
               rep("Quality Fertilizer",
                   nrow(croptimizer::crops)))
})

test_that("when soil_mod is a singleton, it is repeated", {
  soil_mod_test <-
    append_soil_mod(soil_mods = "quality fertilizer")
  expect_equal(soil_mod_test$soil_mod,
               rep("Quality Fertilizer",
                   nrow(croptimizer::crops)))
})

test_that("when soil_mod is a named vector, it is joined and NA is 'normal'", {
  soil_mods <- c("Blueberry" = "Quality Fertilizer",
                 "Bok Choy" = "Basic Fertilizer",
                 "Cauliflower" = "Deluxe Fertilizer",
                 "Corn" = "Speed-Gro")
  soil_mod_test <-
    append_soil_mod(soil_mods = soil_mods)
  soil_mod_normals <-
    soil_mod_test %>% dplyr::filter(! name %in% c(names(soil_mods)))
  expect_equal(soil_mod_test$soil_mod[soil_mod_test$name == "Blueberry"],
               "Quality Fertilizer")
  expect_equal(soil_mod_test$soil_mod[soil_mod_test$name == "Bok Choy"],
               "Basic Fertilizer")
  expect_equal(soil_mod_test$soil_mod[soil_mod_test$name == "Cauliflower"],
               "Deluxe Fertilizer")
  expect_equal(soil_mod_test$soil_mod[soil_mod_test$name == "Corn"],
               "Speed-Gro")
  expect_equal(soil_mod_normals$soil_mod,
               rep("Normal", nrow(soil_mod_normals)))
})

test_that("capitalization is normalized", {
  soil_mods <- c("blueberry" = "QUALITY Fertilizer",
                 "Bok Choy" = "BaSiC fErTiLiZeR",
                 "cauliflower" = "deluxe fertilizer",
                 "corn" = "Speed-GRO")
  soil_mod_test <-
    append_soil_mod(soil_mods = soil_mods)
  soil_mod_normals <-
    soil_mod_test %>%
    dplyr::filter(! name %in%
                    c(tools::toTitleCase(tolower(names(soil_mods)))))
  expect_equal(soil_mod_test$soil_mod[soil_mod_test$name == "Blueberry"],
               "Quality Fertilizer")
  expect_equal(soil_mod_test$soil_mod[soil_mod_test$name == "Bok Choy"],
               "Basic Fertilizer")
  expect_equal(soil_mod_test$soil_mod[soil_mod_test$name == "Cauliflower"],
               "Deluxe Fertilizer")
  expect_equal(soil_mod_test$soil_mod[soil_mod_test$name == "Corn"],
               "Speed-Gro")
  expect_equal(soil_mod_normals$soil_mod,
               rep("Normal", nrow(soil_mod_normals)))
})

test_that("if soil_mod named and names non-unique, error results", {
  soil_mods <- c("Blueberry" = "Quality Fertilizer",
                 "Blueberry" = "Deluxe Fertilizer")
  expect_error(append_soil_mod(soil_mods = soil_mods))
})
