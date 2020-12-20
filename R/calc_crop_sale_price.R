#' Calculate Crop Sale Price
#'
#' Calculates the expected value of a crop to sell, taking into account
#' the probability of extra harvests and the number of crops in an extra
#' harvest.
#'
#' @param crop_data data.frame containing crop data, which must contain the
#' columns `sell_price`, `chance_for_extra_crops`, `max_extra_harvest`
#' @param farming_level integer representing current farming level
#' @param level_5_tiller logical representing taking the tiller specialization
#' at level 5
#'
#' @return
#' @export
#'
#' @examples
calc_crop_sale_price <- function(crop_data = croptimizer::crops,
                                 farming_level = 1,
                                 level_5_tiller = NULL) {
  if (! "sell_price" %in% colnames(crop_data)) {
    stop("crop_data must contain column sell_price")
  }

  if (! "chance_for_extra_crops" %in% colnames(crop_data)) {
    stop("crop_data must contain column chance_for_extra_crops")
  }

  if (! "max_extra_harvest" %in% colnames(crop_data)) {
    stop("crop_data must contain column max_extra_harvest")
  }

  if (! "numeric" %in% class(farming_level)) {
    stop("farming_level must be of class numeric")
  }

  if (farming_level < 1) {
    stop("farming_level must be >= 1")
  }

  if (farming_level >= 5 & is.null(level_5_tiller)) {
    stop("if farming_level >= 5, level_5_tiller must be specified")
  }

  if (!is.null(level_5_tiller) && !"logical" %in% class(level_5_tiller)) {
    stop("level_5_tiller must be NULL or of class logical")
  }

  ## Calculate the expected value
  crop_data$exp_sell_price <-
    crop_data$sell_price +
    (crop_data$chance_for_extra_crops *
       crop_data$max_extra_harvest)

  return(crop_data)
}
