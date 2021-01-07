#' Append Soil Mod
#'
#' Appends a column for soil modification (such as fertilizer or speed-gro)
#' to the crop data for later use in calculating what fits in season (using
#' `filter_to_season()` and `filter_to_days()`) and in calculating what the
#' expected sale price is (using `calc_crop_sale_price()`).
#'
#' @param crop_data a data.frame containing the data about the crops,
#' expected to contain the same columns as `croptimizer::crops`
#'
#' @return crop_data with a soil_mod column appended based upon user
#' input if provided, and containing "normal" if not
#' @export
#'
#' @examples
append_soil_mod <- function(crop_data = croptimizer::crops,
                            soil_mods = NULL,
                            soil_mod_names = croptimizer::soil_mod_names) {
  ## Logic:
  ## If soil_mod is null, put "normal" for all
  ## If soil_mod is non-named, must be of length = num rows,
  ## and just append
  ## If names, join on names and for missing values put "normal"

  ## If soil mod is null, just put "normal" for all
  if (is.null(soil_mods)) {
    crop_data$soil_mod <- rep("normal")
    return(crop_data)
  }

  ## Check soil_mod length
  if (is.null(names(soil_mods)) && length(soil_mods) != nrow(crop_data)) {
    error("soil_mod must be named or of length = nrow(crop_data)")
  }

  ## Append the soil_mod
  if (is.null(names(soil_mods))) {
    crop_data$soil_mod <- soil_mods
    return(crop_data)
  }

  ## TODO finish with named soil mods
}
