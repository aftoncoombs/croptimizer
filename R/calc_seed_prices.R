#' Calc Seed Prices
#'
#' @param crop_data data.frame containing crop data, defaults to the data
#' contained in the croptimizer package
#' @param joja_member logical specifying if you have the JojaMart membership
#'
#' @return data.frame containing crop_data with sell_price and source columns
#' appended
#' @export
#'
#' @examples
calc_seed_prices <- function(crop_data = croptimizer::crops,
                             joja_member = FALSE) {
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
                    min(purchased_price_pierre, joja_col, na.rm = TRUE)) %>%
    dplyr::mutate(source = ifelse(test = purchased_price_pierre == seed_price,
                                    yes = "pierre",
                                    no = "jojamart"))

  return(crop_data)
}
