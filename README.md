
<!-- README.md is generated from README.Rmd. Please edit that file -->

# r2dii.plot.static <a href='https://github.com/2DegreesInvesting/r2dii.plot.static'><img src='https://imgur.com/A5ASZPE.png' align='right' height='43' /></a>

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html)
[![Codecov test
coverage](https://codecov.io/gh/2DegreesInvesting/r2dii.plot.static/branch/master/graph/badge.svg)](https://codecov.io/gh/2DegreesInvesting/r2dii.plot.static?branch=master)
[![R-CMD-check](https://github.com/2DegreesInvesting/r2dii.plot.static/workflows/R-CMD-check/badge.svg)](https://github.com/2DegreesInvesting/r2dii.plot.static/actions)
[![CRAN
status](https://www.r-pkg.org/badges/version/r2dii.plot.static)](https://CRAN.R-project.org/package=r2dii.plot.static)
<!-- badges: end -->

The goal of r2dii.plot.static is to provide users with plotting and data
processing functions that will allow the users to create standard 2DII
plots using `PACTA_analysis` or banks’ output data as input. The plots
are in the form of ggplot objects.

## Installation

You can install the development version of r2dii.plot.static from
[GitHub](https://github.com/2DegreesInvesting/r2dii.plot.static) with:

``` r
# install.packages("devtools")
devtools::install_github("2DegreesInvesting/r2dii.plot.static")
```

[How to minimize installation
errors?](https://gist.github.com/maurolepore/a0187be9d40aee95a43f20a85f4caed6#installation)

## Example

``` r
library(tidyverse)
#> ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.1 ──
#> ✓ ggplot2 3.3.3     ✓ purrr   0.3.4
#> ✓ tibble  3.1.2     ✓ dplyr   1.0.6
#> ✓ tidyr   1.1.3     ✓ stringr 1.4.0
#> ✓ readr   1.4.0     ✓ forcats 0.5.1
#> ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
#> x dplyr::filter() masks stats::filter()
#> x dplyr::lag()    masks stats::lag()
library(r2dii.plot.static)
```

-   `example_data` imports example data set for plotting.
-   `process_input_data()` performs the initial processing on raw input
    data in banks’ format.

``` r
example_data <- process_input_data(example_data)
```

-   `plot_trajectory()` create a trajectory alignment chart in a ggplot
    object.

``` r
data_trajectory <- prepare_for_trajectory_chart(
  example_data,
  sector_filter = "power",
  technology_filter = "renewablescap",
  region_filter = "global",
  scenario_source_filter = "demo_2020",
  value_name = "production",
  end_year_filter = 2025,
  normalize_to_start_year = TRUE
)

scenario_specs <- tibble(
  scenario = c("sds", "sps", "cps", "worse"),
  color = c("#9CAB7C", "#FFFFCC", "#FDE291", "#E07B73"),
  label = c("SDS", "STEPS", "CPS", "worse")
)

main_line_metric <- tibble(
  metric = "projected",
  label = "Portfolio"
)

additional_line_metrics <- tibble(
  metric = "corporate_economy",
  label = "Corporate Economy"
)

plot_trajectory(
  data_trajectory,
  scenario_specs_good_to_bad = scenario_specs,
  main_line_metric = main_line_metric,
  additional_line_metrics = additional_line_metrics
)
```

<img src="man/figures/README-unnamed-chunk-4-1.png" width="100%" style="display: block; margin: auto auto auto 0;" />

-   `prepare_for_techmix_chart()` prepares pre-processed data for
    plotting a tech-mix chart.
-   `get_r2dii_technology_colours()` get the predefined technology
    colors for a sector.
-   `plot_techmix()` create a techmix chart in a ggplot object.

``` r
# Default colours, all data, added title

sector <- "power"

data <- prepare_for_techmix_chart(example_data,
  sector_filter = sector,
  years_filter = c(2020, 2025), region_filter = "global",
  scenario_source_filter = "demo_2020",
  scenario_filter = "sds", value_to_plot = "technology_share"
)

plot <- plot_techmix(data)
plot +
  ggplot2::labs(title = "Technology mix for the Power sector")
```

<img src="man/figures/README-unnamed-chunk-5-1.png" width="100%" style="display: block; margin: auto auto auto 0;" />

``` r
# Custom colours, all data, no title
power_colors_custom <- tibble(
  technology = c("coalcap", "oilcap", "gascap", "nuclearcap", "hydrocap", "renewablescap"),
  label = c("Coal Capacity", "Oil Capacity", "Gas Capacity", "Nuclear Capacity", "Hydro Capacity", "Renewables Capacity"),
  colour = c("black", "brown", "grey", "red", "blue", "green4")
)

plot <- plot_techmix(data,
  tech_colours = power_colors_custom
)
plot
```

<img src="man/figures/README-unnamed-chunk-5-2.png" width="100%" style="display: block; margin: auto auto auto 0;" />

``` r
# Default colours, selected data and labels (metric_type parameters), added title

sector <- "automotive"

data <- prepare_for_techmix_chart(example_data,
  sector_filter = sector,
  years_filter = c(2020, 2025), region_filter = "global",
  scenario_source_filter = "demo_2020",
  scenario_filter = "sds", value_to_plot = "technology_share"
)

metric_type_order = c(
    "portfolio_2020", "benchmark_2020", "portfolio_2025",
    "benchmark_2025", "scenario_2025"
)
metric_type_labels = c(
    "Portfolio 2020", "Benchmark 2020", "Portfolio 2025",
    "Benchmark 2025", "Target SDS 2025"
  )

plot <- plot_techmix(data,
  metric_type_order = metric_type_order,
  metric_type_labels = metric_type_labels
)
plot +
  ggplot2::labs(title = "Technology mix for the Automotive sector")
```

<img src="man/figures/README-unnamed-chunk-5-3.png" width="100%" style="display: block; margin: auto auto auto 0;" />

-   `prepare_for_timeline()` prepares sda\_target-type data for timeline
    plot.
-   `plot_timelineA()` creates a time line plot.

``` r
# Using default preparation and specs
data <- prepare_for_timeline(sda_target)
#> Warning: Can only use one sector.
#> Using the first of the vector passed to `sector_filter`: automotive.
plot_timelineA(data)
```

<img src="man/figures/README-unnamed-chunk-6-1.png" width="100%" style="display: block; margin: auto auto auto 0;" />

``` r
# Using custom preparation
data <- prepare_for_timeline(
  sda_target,
  sector_filter = "cement",
  year_start = 2020,
  year_end = 2050,
  column_line_names = "emission_factor_metric",
  value_to_plot = "emission_factor_value",
  extrapolate_missing_values = TRUE
)
#> Warning: Can only use one sector.
#> Using the first of the vector passed to `sector_filter`: cement.

# Using custom specs and extending the plot with ggplot2
plot_timelineA(data) +
  labs(
    title = "Emission intensity trend for Cement.",
    x = "Year",
    y = "Tons of CO2 per ton",
    caption = "Dashed line is an extrapolation of the last value in the dataset."
  )
```

<img src="man/figures/README-unnamed-chunk-6-2.png" width="100%" style="display: block; margin: auto auto auto 0;" />

-   `timeline_specs()` creates the default specs data frame for
    ‘plot\_timelinea()’.
-   `r2dii_palette_colours()` outputs a data frame giving the 2dii
    colour palette.

``` r
# You may use it as a template to create your custom specs
timeline_specs(data)
#> # A tibble: 4 x 3
#>   line_name              label                  colour_hex
#>   <chr>                  <chr>                  <chr>     
#> 1 projected              Projected              #1b324f   
#> 2 corporate_economy      Corporate Economy      #00c082   
#> 3 target_demo            Target Demo            #ff9623   
#> 4 adjusted_scenario_demo Adjusted Scenario Demo #d0d7e1

# You may use it as a reference for 2DII's colour palette
r2dii_palette_colours()
#> # A tibble: 9 x 2
#>   label       colour_hex
#>   <chr>       <chr>     
#> 1 dark_blue   #1b324f   
#> 2 green       #00c082   
#> 3 orange      #ff9623   
#> 4 grey        #d0d7e1   
#> 5 dark_purple #574099   
#> 6 yellow      #f2e06e   
#> 7 soft_blue   #78c4d6   
#> 8 ruby_red    #a63d57   
#> 9 moss_green  #4a5e54
```
