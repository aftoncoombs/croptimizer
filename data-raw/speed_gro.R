## Creates speed-gro proportions which determine changes to growth
## TODO: Update with sourced game code or data files instead
## of manual entry

speed_gro <-
  data.frame(soil_mod = c("Speed-Gro",
                          "Deluxe Speed-Gro",
                          "Hyper Speed-Gro"),
             effect = c(0.9,
                        0.75,
                        0.66))
usethis::use_data(speed_gro, overwrite = TRUE)
