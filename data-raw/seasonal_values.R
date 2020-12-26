## Establishes days per season and seasons per year

seasonal_values <-
  list(days_per_season = 28,
       seasons_per_year = 4,
       min_day_of_season = 1)

usethis::use_data(seasonal_values, overwrite = TRUE)
