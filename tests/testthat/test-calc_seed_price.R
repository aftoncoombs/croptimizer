## Runs tests for calc_seed_prices()

test_that("error when joja_member is not class logical", {
  expect_error(calc_seed_prices(joja_member = "boba"))
  expect_error(calc_seed_prices(joja_member = 42))
})

test_that("error when crop_data is not class data.frame", {
  expect_error(calc_seed_prices(crop_data = "boba"))
  expect_error(calc_seed_prices(crop_data = 42))
})

test_that("correctly finds minimum prices", {
  no_joja <-
    calc_seed_prices() %>%
    dplyr::rowwise() %>%
    dplyr::mutate(seed_min_for_checking =
                    min(purchased_price_pierre,
                        purchased_price_joja_no_membership))

  has_joja <-
    calc_seed_prices(joja_member = TRUE) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(seed_min_for_checking =
                    min(purchased_price_pierre,
                        purchased_price_joja_with_membership))

  expect_equal(no_joja$seed_price, no_joja$seed_min_for_checking)

  expect_equal(has_joja$seed_price, has_joja$seed_min_for_checking)
})

test_that("correctly finds source of minimum prices", {
  no_joja <-
    calc_seed_prices() %>%
    dplyr::mutate(source_for_checking =
                    ifelse(test = !is.na(seed_price),
                           yes =
                             ifelse(test =
                                      seed_price ==
                                      purchased_price_joja_no_membership &
                                      seed_price ==
                                      purchased_price_pierre,
                                    yes = "pierre, jojamart",
                                    no = ifelse(test =
                                                  seed_price ==
                                                  purchased_price_joja_no_membership,
                                                yes = "jojamart",
                                                no = "pierre")),
                           no = NA))

  has_joja <-
    calc_seed_prices(joja_member = TRUE) %>%
    dplyr::mutate(source_for_checking =
                    ifelse(test = !is.na(seed_price),
                           yes =
                             ifelse(test =
                                      seed_price ==
                                      purchased_price_joja_with_membership &
                                      seed_price ==
                                      purchased_price_pierre,
                                    yes = "pierre, jojamart",
                                    no = ifelse(test =
                                                  seed_price ==
                                                  purchased_price_joja_with_membership,
                                                yes = "jojamart",
                                                no = "pierre")),
                           no = NA))

  expect_equal(no_joja$source, no_joja$source_for_checking)

  expect_equal(has_joja$source, has_joja$source_for_checking)
})

test_that("columns are dropped if drop_cols == TRUE", {
  drop_cols_test <- calc_seed_prices(drop_cols = TRUE)

  expect_false("purchased_price_pierre" %in% colnames(drop_cols_test))
  expect_false("purchased_price_joja_no_membership" %in% colnames(drop_cols_test))
  expect_false("purchased_price_joja_with_membership" %in% colnames(drop_cols_test))
})

test_that("columns are not dropped if drop_cols == FALSE", {
  drop_cols_test <- calc_seed_prices()

  expect_true("purchased_price_pierre" %in% colnames(drop_cols_test))
  expect_true("purchased_price_joja_no_membership" %in% colnames(drop_cols_test))
  expect_true("purchased_price_joja_with_membership" %in% colnames(drop_cols_test))
})
