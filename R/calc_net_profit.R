#' Calculate Net Profit
#'
#' Calculates the net profit for a crop by subtracting the cost of
#' the seeds from the sale price of the crop. Does not currently
#' account for the cost of soil modifiers such as fertilizer and
#' speed-gro, and does not account for seeds potentially obtained
#' for free.
#'
#' Crop data is required to contain columns `seed_price` and
#' `expected_sell_price`.
#'
#' @param crop_data crop data to sort, must contain columns `seed_price` and
#' `expected_sell_price`
#' @param sort a logical indicating if you would like the results sorted by net
#' profit
#'
#' @return crop_data with a net profit column appended
#' @export
#'
#' @examples
calc_net_profit <- function(crop_data, sort = TRUE) {
  if (! ("seed_price" %in% colnames(crop_data) &
         "exp_sell_price" %in% colnames(crop_data))) {
    stop("crop_data must contain columns seed_price and exp_sell_price")
  }

  if (! "logical" %in% class(sort)) {
    stop("sort must be of class logical")
  }

  crop_data <-
    crop_data %>%
    dplyr::mutate(net_profit = exp_sell_price - seed_price)

  if (sort) {
    crop_data <-
      crop_data %>%
      dplyr::arrange(desc(net_profit))
  }

  return(crop_data)
}
