## Load crops data

## Example flow of functions
## 1) Start with crop data in package
## 2) calc base seed price with calc_seed_prices()
## 3) append soil modification (e.g. speed gro, fert)
## 4) filter_to_season()/filter_to_days()
## 5) get sale price with calc_crop_sale_price()

## Little TODO's
## TODO: Fix error in append soil mod that only allows joining on names
## if a single element (doesn't work for multiples) and also update tests
## TODO: Add checking for non-standard fertilizers and speed gro
## (specifically in the case of a non-named vector)
## TODO: Verify that speed gro results are floored
## TODO: Fix inefficient calc_crop_sale_price by actually selecting the joja_col
## TODO: Work on the vignette

## Medium TODO's
## TODO: Account for the cost of soil modifiers
## TODO: add optimize_soil_mod() function
## TODO: take package data out of user-accessible params

## Big TODO's
## TODO: Accept a list of items you already have, and mark their price as 0
## TODO: Pass the size of available land
## TODO: maybe? address contiguous crops
## TODO: add in specialty shop crops and seeds

crops <-
  rstardew::crops %>%
  dplyr::left_join(y = rstardew::seeds_object_information,
                   by = "objectid") %>%
  dplyr::left_join(y = rstardew::crops_object_information,
                   by = c("index_of_harvest" = "objectid"))

crops$total_days_in_growth <- rowSums(crops[ , grepl(pattern = "^days_in_stage_", x = colnames(crops))], na.rm = TRUE)
crops$total_num_seasons <-
  rowSums(crops[ , grepl(pattern = "^growth_season_", x = colnames(crops))], na.rm = TRUE)
crops$total_days_in_growth_season <-
  crops$total_num_seasons * seasonal_values$days_per_season
crops$total_days_in_growth_season <-
  ifelse(test = crops$total_days_in_growth_season ==
           seasonal_values$days_per_season * seasonal_values$seasons_per_year,
         yes = Inf,
         no = crops$total_days_in_growth_season)

crops <-
  crops %>%
  dplyr::select(name = display_name.y,
                total_num_seasons, total_days_in_growth,
                total_days_in_growth_season, growth_season_spring,
                growth_season_summer, growth_season_fall,
                growth_season_winter, regrow_after_harvest,
                chance_for_extra_harvest, min_extra_harvest,
                max_extra_harvest, max_harvest_increase_per_farming_level,
                chance_for_extra_crops, raised_seeds,
                seed_sell_price = price.x,
                sell_price = price.y) %>%
  dplyr::filter(!is.na(name)) %>%
  dplyr::mutate(chance_for_extra_crops =
                  tidyr::replace_na(chance_for_extra_crops, replace = 0),
                max_harvest_increase_per_farming_level =
                  tidyr::replace_na(max_harvest_increase_per_farming_level,
                                    replace = 0),
                min_extra_harvest =
                  tidyr::replace_na(min_extra_harvest, replace = 0),
                max_extra_harvest =
                  tidyr::replace_na(max_extra_harvest, replace = 0),
                purchased_price_pierre = 2 * seed_sell_price,
                purchased_price_joja_no_membership = floor(2.5 * seed_sell_price)) %>%
  dplyr::mutate(purchased_price_joja_with_membership = purchased_price_pierre) %>%
  dplyr::mutate(purchased_price_pierre =
                  ifelse(test = name == "Sunflower",
                         yes = 200,
                         no = purchased_price_pierre),
                purchased_price_joja_no_membership =
                  ifelse(test = name == "Sunflower",
                         yes = 125,
                         no = purchased_price_joja_no_membership),
                purchased_price_joja_with_membership =
                  ifelse(test = name == "Sunflower",
                         yes = 100,
                         no = purchased_price_joja_with_membership)) %>%
  dplyr::mutate(purchased_price_pierre =
                  ifelse(test = purchased_price_pierre == 0,
                         yes = NA,
                         no = purchased_price_pierre),
                purchased_price_joja_no_membership =
                  ifelse(test = purchased_price_joja_no_membership == 0,
                         yes = NA,
                         no = purchased_price_joja_no_membership),
                purchased_price_joja_with_membership =
                  ifelse(test = purchased_price_joja_with_membership == 0,
                         yes = NA,
                         no = purchased_price_joja_with_membership)) %>%
  as.data.frame()

usethis::use_data(crops, overwrite = TRUE)
