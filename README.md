
<!-- README.md is generated from README.Rmd. Please edit that file -->

# croptimizer

<!-- badges: start -->

<!-- badges: end -->

The goal of croptimizer is to optimize the crops planted in Stardew
Valley\! It optimizes crops, therefore – a croptimizer. 😬

## Installation

This package is in process, and as such it is not stable and may change
frequently. However, if you would like to install it, then do so from
this repo, e.g. using `devtools` or any alternative package that offers
an `install_github()` function.

``` r
devtools::install_github(repo = "aftonsteps/croptimizer", ref = "main")
```

## Overview

The goal of this project is to allow optimization of your Stardew Valley
crop choices. Users should be able to provide information such as number
of squares available, season, and day of season, and receive the optimal
crops to choose for these squares. Optimization will select crops that
have sufficient time to grow before season end and then choose among
these those which yield the most predicted gold.

The loose plan for building this croptimizer is to handle it in a few
phases:

  - Initial phase: Optimize for most profitable crop in terms of gold
    per day, which is (expected value of crop yield - initial cost of
    seeds) / number of days required for growing. This will optionally
    limit results to those which are possible in the time remaining
    (i.e. only crops which have enough time to mature before the end of
    the season). This should also take into account the potential
    bonuses due to probability of quality crop, probability of multiple
    harvest, and increases due to level and/or agriculturalist
    profession.
  - Later features: Optimizing for additional parameters such as
    allowing arbitrary target date and not just end-of-season,
    accounting for free items such as foraged and dropped seeds which
    have an upfront cost of 0 gold, accounting for soil attributes such
    as fertilizer.
  - “Stretch-goal” features: Optimizing for crop arrangement as well,
    which will take into account the number of soil squares available.
    This will allow optimization for giant crop probabilities and will
    prevent impossible arrangements such as planting all raised crops,
    which would be accessible only at the perimeter.
