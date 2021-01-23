#' Update Total Days in Growth
#'
#' Updates the `total_days_in_growth` column in crops data to account
#' for decreased days in growth due to applying Speed-Gro. Crop data
#' must contain the column `soil_mod`, indicating what fertilizer of speed-gro
#' is applied to that crop.
#'
#' @return crop data with updated `total_days_in_growth` which accounts for
#' Speed-Gro
#' @export
#'
#' @examples
#' ## Hyper Speed-Gro
#' speed_gro <-
#' append_soil_mod(soil_mods = "hyper speed-gro") %>%
#' update_total_days_in_growth()
#' ## Deluxe Speed-Gro
#' speed_gro <-
#' append_soil_mod(soil_mods = "deluxe speed-gro") %>%
#' update_total_days_in_growth()
update_total_days_in_growth <- function(crop_data, level_10_agri = NULL) {

  if (!is.null(level_10_agri) && !"logical" %in% class(level_10_agri)) {
    stop("level_10_agri must be null or of class logical")
  }

  ## If agriculturalist profession, total_days_in_growth * .9
  if (!is.null(level_10_agri) && level_10_agri == TRUE) {
    crop_data$total_days_in_growth <-
      ceiling(crop_data$total_days_in_growth * 0.9)
  }

  ## If soil mod column exists, apply bonus for soil mods
  if ("soil_mod" %in% colnames(crop_data)) {
    crop_data <-
      crop_data %>%
      dplyr::left_join(y = croptimizer::speed_gro, by = c("soil_mod" = "soil_mod")) %>%
      dplyr::mutate(effect = ifelse(test = is.na(effect),
                                    yes = 1,
                                    no = effect)) %>%
      dplyr::mutate(total_days_in_growth = floor(total_days_in_growth * effect))
  }

  return(crop_data)
}
