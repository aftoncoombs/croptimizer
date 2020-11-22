#' Filter To Season
#'
#' @param season character containing current season
#' @param crop_data data.frame containing crop data, defaults to package data `crops`
#'
#' @return data.frame containing crop data filtered to current season crops
#' @export
#'
#' @examples
#' ## Filters to fall season
#' filter_to_season(season = "fall")
filter_to_season <- function(season, crop_data = croptimizer::crops) {
  ## Error checking
  if (class(season) != "character" | length(season) > 1) {
    stop("season must be character of length 1")
  }

  if (! season %in% c("spring", "summer", "fall", "winter")) {
    stop("season must be one of spring, summer, fall, or winter")
  }

  if (!"data.frame" %in% class(crop_data)) {
    stop("crop data must be of class data.frame")
  }

  ## Filter to crops that are in the selected season
  col_to_select <- paste0("growth_season_", season)
  subset_vec <- crop_data[, col_to_select] == TRUE
  filtered_data <- crop_data[subset_vec, ]

  return(filtered_data)
}
