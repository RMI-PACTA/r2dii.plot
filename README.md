
<!-- README.md is generated from README.Rmd. Please edit that file -->

# r2dii.plot <a href='https://github.com/2DegreesInvesting/r2dii.plot'><img src='https://imgur.com/A5ASZPE.png' align='right' height='43' /></a>

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html)
[![Codecov test
coverage](https://codecov.io/gh/2DegreesInvesting/r2dii.plot/branch/master/graph/badge.svg)](https://codecov.io/gh/2DegreesInvesting/r2dii.plot?branch=master)
[![R-CMD-check](https://github.com/2DegreesInvesting/r2dii.plot/workflows/R-CMD-check/badge.svg)](https://github.com/2DegreesInvesting/r2dii.plot/actions)
[![CRAN
status](https://www.r-pkg.org/badges/version/r2dii.plot)](https://CRAN.R-project.org/package=r2dii.plot)
<!-- badges: end -->

The goal of r2dii.plot is to help you plot 2DII data in an informative,
beautiful, and easy way.

## Installation

You can install the development version of r2dii.plot from
[GitHub](https://github.com/2DegreesInvesting/r2dii.plot) with:

``` r
# install.packages("devtools")
devtools::install_github("2DegreesInvesting/r2dii.plot")
```

## Example

The r2dii.plot package is designed to work smoothly with other “r2dii”
packages –
[r2dii.data](https://2degreesinvesting.github.io/r2dii.data/),
[r2dii.match](https://2degreesinvesting.github.io/r2dii.match/), and
[r2dii.analysis](https://2degreesinvesting.github.io/r2dii.analysis/).
It also plays well with the popular packages
[dplyr](https://www.tidyverse.org/) and
[ggplot2](https://ggplot2.tidyverse.org/), which help you customize your
plots.

``` r
library(dplyr, warn.conflicts = FALSE)
library(ggplot2, warn.conflicts = FALSE)
library(r2dii.plot)
```

Your data typically comes from the output of two functions in the
r2dii.analysis package:
[`target_sda()`](https://2degreesinvesting.github.io/r2dii.analysis/reference/target_sda.html)
and
[`target_market_share()`](https://2degreesinvesting.github.io/r2dii.analysis/reference/target_market_share.html).
Here you’ll use two example datasets that come with r2dii.plot.

``` r
sda
#> # A tibble: 208 x 4
#>    sector      year emission_factor_metric emission_factor_value
#>    <chr>      <dbl> <chr>                                  <dbl>
#>  1 automotive  2002 projected                              0.228
#>  2 automotive  2003 projected                              0.226
#>  3 automotive  2004 projected                              0.224
#>  4 automotive  2005 projected                              0.222
#>  5 automotive  2006 projected                              0.220
#>  6 automotive  2007 projected                              0.218
#>  7 automotive  2008 projected                              0.216
#>  8 automotive  2009 projected                              0.214
#>  9 automotive  2010 projected                              0.212
#> 10 automotive  2011 projected                              0.210
#> # … with 198 more rows

market_share
#> # A tibble: 1,170 x 8
#>    sector     technology  year region scenario_source metric          production
#>    <chr>      <chr>      <int> <chr>  <chr>           <chr>                <dbl>
#>  1 automotive electric    2020 global demo_2020       projected          145942.
#>  2 automotive electric    2020 global demo_2020       corporate_econ…   8134869.
#>  3 automotive electric    2020 global demo_2020       target_cps         145942.
#>  4 automotive electric    2020 global demo_2020       target_sds         145942.
#>  5 automotive electric    2020 global demo_2020       target_sps         145942.
#>  6 automotive electric    2021 global demo_2020       projected          148212.
#>  7 automotive electric    2021 global demo_2020       corporate_econ…   8183411.
#>  8 automotive electric    2021 global demo_2020       target_cps         148361.
#>  9 automotive electric    2021 global demo_2020       target_sds         160625.
#> 10 automotive electric    2021 global demo_2020       target_sps         149016.
#> # … with 1,160 more rows, and 1 more variable: technology_share <dbl>
```

r2dii.plot currently supports three kinds of plots: `plot_timeline*()`,
`plot_techmix*()`, and `plot_trajectory*()`. Each plot has specific
requirements about the main input – passed to the first argument `data`.
To meet those requirements we currently provide two experimental sets of
functions ([API](https://en.wikipedia.org/wiki/API)s) – “X” and “Y”.
Both APIs can help you get the same basic plots, which you can further
customization with ggplot2. Their difference difference is not in what
you can do but in how you can do it:

  - With the “X” API you meet the `data` requirements mainly with
    `dplyr::filter()`, and with “internal magic” based on the known
    structure of r2dii data. This API should be best for users who
    already use dplyr or want to learn it. It allows for slightly less
    customization than “Y” but with the advantage of a much simpler
    interface.

  - With the “Y” API you could meet the `data` requirements with dplyr
    but you can also use dedicated “preparation” functions
    (`prep_*Y()`). There are explicit arguments to both the preparation
    and plotting functions which allow for a customization inside the
    plotting function of what appears in the plot and how (colours,
    labels). This API should be best for users who do not use dplyr or
    care about it.

Users and developers may have different preferences. The tables below
compare the X and Y APIs across a number of criteria relevant to them.

| Criteria                                | Thin API “X”                     | Thin API, “Y”                                 |
| :-------------------------------------- | :------------------------------- | :-------------------------------------------- |
| Required knowledge of dplyr and ggplot2 | More                             | Less                                          |
| Customization possible                  | Limitless with dplyr and ggplot2 | Limitless with dplyr, ggplot2, and r2dii.plot |
| Integration with other R workflows      | More                             | Less                                          |

The X and Y APIs compared from a user’s perspective.

| Criteria           | Thin API “X” | Thin API, “Y” |
| :----------------- | :----------- | :------------ |
| Maintenance burden | Less         | More          |
| Easy to extend     | More         | Less          |

The X and Y APIs compared from a developer’s perspective.

To make the comparison concrete consider this small example of a
trajectory plot (the other plot types you can find in the detailed [“X”
API](https://2degreesinvesting.github.io/r2dii.plot/articles/articles/r2dii-plot-X.html)
and [“Y”
API](https://2degreesinvesting.github.io/r2dii.plot/articles/articles/r2dii-plot-Y.html))
articles. Notice the resulting plot is almost the same (except for the
labels) but the toolkit is different.

  - “X” API

<!-- end list -->

``` r
data <- market_share

prep <- filter(
  data,
  sector == "power",
  technology == "renewablescap",
  region == "global",
  scenario_source == "demo_2020",
  year <= 2025
)

plot_trajectoryX(prep) + 
  labs (title = "Trajectory plot with the thin 'X' API")
```

<img src="man/figures/README-unnamed-chunk-6-1.png" width="100%" style="display: block; margin: auto auto auto 0;" />

  - “Y” API

<!-- end list -->

``` r
data <- market_share

prep <- prep_trajectoryY(
  data,
  sector_filter = "power",
  technology_filter = "renewablescap",
  region_filter = "global",
  scenario_source_filter = "demo_2020",
  value = "production"
)

scenario_specs <- dplyr::tibble(
  scenario = c("sds", "sps", "cps"),
  label = c("SDS", "STEPS", "CPS")
)

main_line_metric <- dplyr::tibble(metric = "projected", label = "Portfolio")

additional_line_metrics <- dplyr::tibble(
  metric = "corporate_economy",
  label = "Corporate Economy"
)

plot_trajectoryY(
  prep,
  scenario_specs_good_to_bad = scenario_specs,
  main_line_metric = main_line_metric,
  additional_line_metrics = additional_line_metrics
) + 
  labs (title = "Trajectory plot with the thick 'Y' API")
```

<img src="man/figures/README-unnamed-chunk-7-1.png" width="100%" style="display: block; margin: auto auto auto 0;" />

For full examples see the dedicated articles [r2dii.plot
X](https://2degreesinvesting.github.io/r2dii.plot/articles/articles/r2dii-plot-X.html)
and [r2dii.plot
Y](https://2degreesinvesting.github.io/r2dii.plot/articles/articles/r2dii-plot-Y.html).
