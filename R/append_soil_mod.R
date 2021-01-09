#' Append Soil Mod
#'
#' Appends a column for soil modification (such as fertilizer or speed-gro)
#' to the crop data for later use in calculating what fits in season (using
#' `filter_to_season()` and `filter_to_days()`) and in calculating what the
#' expected sale price is (using `calc_crop_sale_price()`).
#'
#' If soil_mods are NULL, "Normal" is repeated. If soil_mods is a singleton,
#' it is repeated. If soil_mods is a non-name vector, cbind is attempted.
#' If soil_mod is a named vector, names should be a crop name (e.g. "Blueberry")
#' and a left join will be attempted.
#'
#' @param crop_data a data.frame containing the data about the crops,
#' expected to contain the same columns as `croptimizer::crops`
#' @param soil_mods accepts NULL, a singleton character for a soil
#' modifier, a vector of length = nrow(crop_data) without names, or
#' a names vector of any length where names are unique and represent
#' crop modifiers
#'
#' @return crop_data with a soil_mod column appended based upon user
#' input if provided, and containing "Normal" if not
#' @export
#'
#' @examples
#' ## Example with named soil mods
#'   soil_mods <- c("Blueberry" = "Quality Fertilizer",
#'   "Bok Choy" = "Basic Fertilizer",
#'   "Cauliflower" = "Deluxe Fertilizer",
#'   "Corn" = "Speed-Gro")
#'   soil_mod_data <-
#'   append_soil_mod(soil_mods = soil_mods)
append_soil_mod <- function(crop_data = croptimizer::crops,
                            soil_mods = NULL,
                            soil_mod_names = croptimizer::soil_mod_names) {

  ACCEPTED_SOIL_MODS <-
    c("Normal",
      "Basic Fertilizer",
      "Basic Retaining Soil",
      "Deluxe Fertilizer",
      "Deluxe Retaining Soil",
      "Deluxe Speed-Gro",
      "Hyper Speed-Gro",
      "Quality Fertilizer",
      "Quality Retaining Soil",
      "Speed-Gro")

  ## Assume "Normal" if soil mod provided is NULL
  if (is.null(soil_mods)) {
    crop_data$soil_mod <- rep("Normal")
    return(crop_data)
  }

  ## Rep if just a singleton soil mod
  if (length(soil_mods) == 1) {
    crop_data$soil_mod <- rep(x = tools::toTitleCase(tolower(soil_mods)))
    return(crop_data)
  }

  ## Check for legit soil mods
  if (sum(tools::toTitleCase(tolower(soil_mods)) %in% ACCEPTED_SOIL_MODS) !=
      length(soil_mods)) {
    stop("non-standard soil mod name")
  }

  ## Check for unique names, if named
  if (length(unique(names(soil_mods))) != length(names(soil_mods))) {
    stop("if named, soil_mods names must be unique")
  }

  ## Check soil_mod length
  if (is.null(names(soil_mods)) && length(soil_mods) != nrow(crop_data)) {
    stop("soil_mod must be named or of length = nrow(crop_data)")
  }

  ## Just append if non-named soil mod of length > 1
  if (is.null(names(soil_mods))) {
    crop_data$soil_mod <- tools::toTitleCase(tolower(soil_mods))
    return(crop_data)
  }

  ## If named soil mod, join on names
  if (!is.null(names(soil_mods))) {
    soil_mods <-
      as.data.frame(soil_mods,
                    stringsAsFactors = FALSE) %>%
      tibble::rownames_to_column(var = "name") %>%
      dplyr::select(name, soil_mod = soil_mods) %>%
      dplyr::mutate(name = tools::toTitleCase(tolower(name)),
                    soil_mod = tools::toTitleCase(tolower(soil_mod)))

    crop_data <-
      crop_data %>%
      dplyr::left_join(y = soil_mods, by = "name") %>%
      dplyr::mutate(soil_mod = ifelse(test = is.na(soil_mod),
                                      yes = "Normal",
                                      no = soil_mod))

    return(crop_data)
  }
}
