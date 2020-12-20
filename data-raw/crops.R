## Load crops data
DAYS_PER_SEASON <- 28
SEASONS_PER_YEAR <- 4

## TODO check out wanderer.io
## TODO add more params to calc_crop_sale_price
## - coerce numeric to integer if possible for farming level (fix 1 broken test)
## - do you have level 5 tiller skill
## - farming level (is there a max farming level?)
## - fertilizer/speed grow (probably appended via calc_most_efficient_soil_mod)
## - which level 10 skill (agriculturalist or artisan)
## TODO finish writing tests for calc_crop_sale_price
## TODO add constants
## TODO write net profit
## TODO write calc_most_efficient_soil_mod
## TODO fix inefficient calc_crop_sale_price by actually selecting the joja col

crops <-
  rstardew::crops %>%
  dplyr::left_join(y = rstardew::seeds_object_information,
                   by = "objectid") %>%
  dplyr::left_join(y = rstardew::crops_object_information,
                   by = c("index_of_harvest" = "objectid"))

crops$total_days_in_growth <- rowSums(crops[ , grepl(pattern = "^days_in_stage_", x = colnames(crops))], na.rm = TRUE)
crops$total_num_seasons <-
  rowSums(crops[ , grepl(pattern = "^growth_season_", x = colnames(crops))], na.rm = TRUE)
crops$total_days_in_growth_season <- crops$total_num_seasons * DAYS_PER_SEASON
crops$total_days_in_growth_season <-
  ifelse(test = crops$total_days_in_growth_season ==
           DAYS_PER_SEASON * SEASONS_PER_YEAR,

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
