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
                                 farming_level = 0,
                                 level_5_tiller = NULL,
                                 soil_mod = NULL) {
  if (! "sell_price" %in% colnames(crop_data)) {
    stop("crop_data must contain column sell_price")
  }

  if (! "chance_for_extra_crops" %in% colnames(crop_data)) {
    stop("crop_data must contain column chance_for_extra_crops")
  }

  if (! "max_extra_harvest" %in% colnames(crop_data)) {
    stop("crop_data must contain column max_extra_harvest")
  }

  if (! ("numeric" %in% class(farming_level)) |
      "integer" %in% class(farming_level)) {
    stop("farming_level must be of class numeric or integer")
  }

  if (farming_level < 0) {
    stop("farming_level must be >= 0")
  }

  if (farming_level > 13) {
    stop("farming_level must not exceed 10")
  }

  if (farming_level >= 5 & is.null(level_5_tiller)) {
    stop("if farming_level >= 5, level_5_tiller must be specified")
  }

  if (!is.null(level_5_tiller) && !"logical" %in% class(level_5_tiller)) {
    stop("level_5_tiller must be NULL or of class logical")
  }

  if (farming_level - as.integer(farming_level) > 0) {
    warning("farming level coerced to class integer")
  }

  if (!is.null(soil_mod) & !"character" %in% class(soil_mod)) {
    error("soil_mod must be NULL or of type character")
  }

  ## Standardize the crop names if soil_mod is non-null
  if (!is.null(soil_mod)) {
    names(soil_mod) <- tolower(names(soil_mod))
  }

  ## Check to make sure names are all actual crop names
  if (sum(names(soil_mod) %in% tolower(crops$name)) !=
      length(unique(names(soil_mod)))) {
    non_std_names <-
      paste0(names(soil_mod)[! names(soil_mod) %in% tolower(crops$name)],
             collapse = ",")
    error(paste0("non standard soil_mod names:", non_std_names))
  }

  farming_level <- as.integer(farming_level)

  ## Add 10% tiller bonus
  tiller_bonus <-
    ifelse(test = !is.null(level_5_tiller) && level_5_tiller == TRUE,
           yes = 1.1,
           no = 1)

  ## Calculate the expected number of crops
  crop_data$exp_num_crops <-
    ifelse(test = crop_data$chance_for_extra_crops == 0,
           yes = 1,
           no = crop_data$min_extra_harves + crop_data$chance_for_extra_crops)

  ## Calculate the expected value
  crop_data$exp_sell_price <-
    crop_data$sell_price  *
    crop_data$exp_num_crops *
    tiller_bonus

  return(crop_data)
}
