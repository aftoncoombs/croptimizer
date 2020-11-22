## Load crops data

crops <-
  rstardew::crops %>%
  dplyr::left_join(y = rstardew::seeds_object_information %>%
                     dplyr::select(purchase_price = price, dplyr::everything()),
                   by = "objectid") %>%
  dplyr::mutate(name = gsub(pattern = " Seeds$| Bean$| Starter$", replacement = "", x = name)) %>%
  dplyr::left_join(y = rstardew::crops_object_information %>%
                     dplyr::select(sell_price = price, dplyr::everything()),
                   by = "name")

crops$total_days_in_growth = rowSums(crops[ , grepl(pattern = "^days_in_stage_", x = colnames(crops))], na.rm = TRUE)

crops <-
  crops %>%
  dplyr::select(name, total_days_in_growth, growth_season_spring, growth_season_summer, growth_season_fall,
                growth_season_winter, regrow_after_harvest, chance_for_extra_harvest, min_extra_harvest,
                max_extra_harvest, max_harvest_increase_per_farming_level, chance_for_extra_crops, raised_seeds,
                purchase_price, sell_price) %>%
  as.data.frame()

usethis::use_data(crops, overwrite = TRUE)
