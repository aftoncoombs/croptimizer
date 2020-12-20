#' Calc Seed Prices
#'
#' @param crop_data data.frame containing crop data, defaults to the data
#' contained in the croptimizer package
#' @param joja_member logical specifying if you have the JojaMart membership
#'
#' @return data.frame containing crop_data with seed_price and source columns
#' appended
#' @export
#'
#' @examples
#' fall_crops <-
#' filter_to_season(season = "fall", crop_data = croptimizer::crops)
#' calc_seed_prices(crop_data = fall_crops,
#' joja_member = FALSE,
#' drop_cols = TRUE)
calc_seed_prices <- function(crop_data = croptimizer::crops,
                             joja_member = FALSE,
                             drop_cols = FALSE) {
  if (! "logical" %in% class(joja_member)) {
    stop("joja_member must be of class logical")
  }

  if (! "data.frame" %in% class(crop_data)) {
    stop("crop_data must be of class data.frame")
  }

  if (joja_member) {
    joja_col <- crop_data[["purchased_price_joja_with_membership"]]
  } else {
    joja_col <- crop_data[["purchased_price_joja_no_membership"]]
  }

  crop_data <-
    crop_data %>%
    cbind(joja_col) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(seed_price =
                    ifelse(test =
                             !is.na(purchased_price_pierre) & !is.na(joja_col),
                           yes = min(purchased_price_pierre,
                                     joja_col,
                                     na.rm = TRUE),
                           no = NA)) %>%
    dplyr::mutate(source = ifelse(test = !is.na(seed_price),
                                  yes = ifelse(test =
                                                 purchased_price_pierre ==
                                                 seed_price &
                                                 joja_col == seed_price,
                                               yes = "pierre, jojamart",
                                               no = ifelse(test =
                                                             purchased_price_pierre ==
                                                             seed_price,
                                                           yes = "pierre",
                                                           no = "jojamart")),
                                               no = NA)) %>%
                    dplyr::select(-joja_col)

  ## Drop cols
  if (drop_cols) {
    crop_data <-
      crop_data %>%
      dplyr::select(-purchased_price_pierre,
                    -purchased_price_joja_no_membership,
                    -purchased_price_joja_with_membership)
  }

  crop_data <- as.data.frame(crop_data)

  return(crop_data)
}
