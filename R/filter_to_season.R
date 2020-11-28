#' Filter To Season
#'
#' Filters crop data to only crops which grow in the specified season.
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
  DAYS_PER_SEASON <- 28

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

  filtered_data$days_remaining_in_growth_season <-
    filtered_data$total_days_in_growth_season

  ## Get the current season column
  season_col <-
    which(colnames(filtered_data) == paste0("growth_season_", season))

  ## For multi-season crops that have passed some of their seasons,
  ## decrement days remaining
  for (r_idx in 1:nrow(filtered_data)) {
    ## If num seasons > 1, decrement by days per season * num past growth seasons
    if (filtered_data$total_num_seasons[r_idx] > 1) {
      ## Loop over 1 to number of season in this row
      for (c_idx in 1:(filtered_data$total_num_seasons[r_idx] - 1)) {
        ## If encounter a TRUE, decrement by that number of seasons
        if (filtered_data[r_idx, season_col - c_idx]) {
          filtered_data$days_remaining_in_growth_season[r_idx] <-
            filtered_data$days_remaining_in_growth_season[r_idx] -
            DAYS_PER_SEASON
        }
      }
    }
  }

  ## TODO need to update walk-back over season columns to wrap around to winter

  return(filtered_data)
}
