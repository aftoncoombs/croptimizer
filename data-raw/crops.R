## Load crops data
DAYS_PER_SEASON <- 28
SEASONS_PER_YEAR <- 4

seed_prices <-
  readr::read_csv("data-raw/seed_prices.csv") %>%
  dplyr::select(name = Name,
                pierre = "Pierre's General Store",
                jojamart = "JojaMart") %>%
  dplyr::filter(!(is.na(pierre) & is.na(jojamart))) %>%
  dplyr::group_by(name) %>%
  dplyr::mutate(seed_price = min(pierre, jojamart, na.rm = TRUE)) %>%
  dplyr::mutate(source = ifelse(test = pierre == seed_price,
                                yes = "pierre",
                                no = "jojamart"))

crops <-
  rstardew::crops %>%
  dplyr::left_join(y = rstardew::seeds_object_information,
                   by = "objectid") %>%
  dplyr::left_join(y = rstardew::crops_object_information,
                   by = c("index_of_harvest" = "objectid")) %>%
  dplyr::left_join(y = seed_prices,
                   by = c("display_name.y" = "name"))

crops$total_days_in_growth <- rowSums(crops[ , grepl(pattern = "^days_in_stage_", x = colnames(crops))], na.rm = TRUE)
crops$total_num_seasons <-
  rowSums(crops[ , grepl(pattern = "^growth_season_", x = colnames(crops))], na.rm = TRUE)
crops$total_days_in_growth_season <- crops$total_num_seasons * DAYS_PER_SEASON
crops$total_days_in_growth_season <- ifelse(test = crops$total_days_in_growth_season == DAYS_PER_SEASON * SEASONS_PER_YEAR,
                                            yes = Inf,
                                            no = crops$total_days_in_growth_season)

crops <-
  crops %>%
  dplyr::select(name = display_name.y, total_num_seasons, total_days_in_growth,
                total_days_in_growth_season, growth_season_spring,
                growth_season_summer, growth_season_fall,
                growth_season_winter, regrow_after_harvest,
                chance_for_extra_harvest, min_extra_harvest,
                max_extra_harvest, max_harvest_increase_per_farming_level,
                chance_for_extra_crops, raised_seeds,
                purchased_price = seed_price, sell_price = price.y) %>%
  dplyr::filter(!is.na(name)) %>%
  dplyr::mutate(chance_for_extra_crops =
                  tidyr::replace_na(chance_for_extra_crops, replace = 0),
                max_harvest_increase_per_farming_level =
                  tidyr::replace_na(max_harvest_increase_per_farming_level,
                                    replace = 0),
                min_extra_harvest =
                  tidyr::replace_na(min_extra_harvest, replace = 0),
                max_extra_harvest =
                  tidyr::replace_na(max_extra_harvest, replace = 0)) %>%
  as.data.frame()

usethis::use_data(crops, overwrite = TRUE)
