#' Croptmize
#'
#' Higher-level function for sorting available crops by most profitable. Most users will want to use
#' this function.
#'
#' @return
#' @export
#'
#' @examples
croptimize <- function(joja_member = FALSE,
                       soil_mods = NULL,
                       season,
                       day_of_season,
                       farming_level = 0,
                       level_5_tiller = NULL,
                       crops = croptimizer::crops) {
  croptimized_data <-
    croptimizer::crops %>%
    calc_seed_prices(joja_member = FALSE, drop_cols = TRUE) %>%
    append_soil_mod(soil_mods = soil_mods) %>%
    filter_to_season(season = season) %>%
    filter_to_days(day_of_season = day_of_season) %>%
    calc_crop_sale_price(farming_level = farming_level, level_5_tiller = level_5_tiller) %>%
    calc_net_profit() %>%
    dplyr::filter(!is.na(net_profit))

  return(croptimized_data)
}
