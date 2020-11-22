## Load crops data
## TODO fix the pumpkin entry which seems to have a mismatched objectid

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
  dplyr::mutate(name = gsub(pattern = " Seeds$| Bean$| Starter$| Shoot$| Bulb$",
                            replacement = "",
                            x = name)) %>%
  dplyr::left_join(y = rstardew::crops_object_information %>%
                     dplyr::select(sell_price = price, dplyr::everything()),
                   by = "name") %>%
  dplyr::left_join(seed_prices, by = c("name"))

crops$total_days_in_growth = rowSums(crops[ , grepl(pattern = "^days_in_stage_", x = colnames(crops))], na.rm = TRUE)

crops <-
  crops %>%
  dplyr::select(name, total_days_in_growth, growth_season_spring, growth_season_summer, growth_season_fall,
                growth_season_winter, regrow_after_harvest, chance_for_extra_harvest, min_extra_harvest,
                max_extra_harvest, max_harvest_increase_per_farming_level, chance_for_extra_crops, raised_seeds,
                purchased_price = seed_price, sell_price) %>%
  as.data.frame()

usethis::use_data(crops, overwrite = TRUE)
