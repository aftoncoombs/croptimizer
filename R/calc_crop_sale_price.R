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
#' ## Calculate crop sale prices if you have tiller specialty
#' calc_crop_sale_price(farming_level = 5, level_5_tiller = TRUE)
#'
#' ## Calculate crop sale price if you did not take tiller
#' calc_crop_sale_price(farming_level = 10, level_5_tiller = FALSE)
calc_crop_sale_price <- function(crop_data = croptimizer::crops,
                                 farming_level = 0,
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
           no = crop_data$min_extra_harvest + crop_data$chance_for_extra_crops)

  ## Calculate numeric soil mod
  if ("soil_mod" %in% colnames(crop_data)) {
    crop_data <-
      crop_data %>%
      dplyr::mutate(num_soil_mod =
                      ifelse(test = soil_mod == "Deluxe Fertilizer",
                             yes = 4,
                             no = ifelse(test = soil_mod == "Quality Fertilizer",
                                         yes = 2,
                                         no = ifelse(test = soil_mod == "Basic Fertilizer",
                                                     yes = 1,
                                                     no = 0))))
  } else { crop_data$num_soil_mod = rep(0) }

  ## Get probabilities
  ## Expected value = 1*P(normal) + 1.25*P(silver) + 1.5*P(gold)
  ## where P(gold) = 0.01 + 0.2 * (lvl/10 + q * (lvl+2)/12)
  ## P(silver) = MIN(2*P(gold),0.75) * (1-P(gold))
  ## P(normal) = 1 - P(silver) - P(gold)
  crop_data$p_gold <-
    0.01 + 0.2 *(farming_level/10 + num_soil_mod * (farming_level+2)/12)

  crop_data$p_silver <-
    min(c(2*crop_data$p_gold, 0.75), na.rm = TRUE) * (1-crop_data$p_gold)

  crop_data$p_normal <- 1 - crop_data$p_gold - crop_data$p_silver

  ## Calculate the expected value
  crop_data$exp_sell_price <-
    crop_data$sell_price * crop_data$exp_num_crops *
    (crop_data$p_normal + 1.25 * crop_data$p_silver + 1.5 * crop_data$p_gold) *
    tiller_bonus #%>%
    #dplyr::select(-p_gold, -p_silver, -p_normal)

  return(crop_data)
}
