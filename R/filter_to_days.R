#' Filter To Days
#'
#' Filters crop dataset to only crops which have sufficient time left to grow.
#'
#' @param day_of_season integer or numeric representing the date (current day
#' number) of current season
#' @param crop_data data.frame containing crop data, defaults to package data
#' `crops`
#'
#' @return data.frame containing crop data filtered to crops which have
#' sufficient time left in the season to grow
#' @export
#'
#' @examples
#' filter_to_days(day = 25)
filter_to_days <- function(day_of_season,
                           crop_data = croptimizer::crops,
                           seasonal_values = croptimizer::seasonal_values) {
  if (is.null(day_of_season)) {
    stop("day_of_season must be non-null")
  }

  if (! class(day_of_season) %in% c("integer", "numeric")) {
    stop("day_of_season must be of class integer or numeric")
  }

  min_day_of_season <- seasonal_values$min_day_of_season
  days_per_season <- seasonal_values$days_per_season
  if (day_of_season < min_day_of_season | day_of_season > days_per_season) {
    stop("day_of_season must be a value >= 1 and <= 28")
  }

  ## Filter data to crops that have time to grow
  filtered_data <-
    crop_data %>%
    dplyr::filter(total_days_in_growth <=
                    days_remaining_in_growth_season - day_of_season + 1)

  return(filtered_data)
}
