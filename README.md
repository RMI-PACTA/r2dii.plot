
<!-- README.md is generated from README.Rmd. Please edit that file -->

# r2dii.ggplot <a href='https://github.com/2DegreesInvesting/r2dii.ggplot'><img src='https://imgur.com/A5ASZPE.png' align='right' height='43' /></a>

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![CRAN
status](https://www.r-pkg.org/badges/version/r2dii.ggplot)](https://CRAN.R-project.org/package=r2dii.ggplot)
[![Codecov test
coverage](https://codecov.io/gh/2DegreesInvesting/r2dii.ggplot/branch/master/graph/badge.svg)](https://codecov.io/gh/2DegreesInvesting/r2dii.ggplot?branch=master)
[![R-CMD-check](https://github.com/2DegreesInvesting/r2dii.ggplot/workflows/R-CMD-check/badge.svg)](https://github.com/2DegreesInvesting/r2dii.ggplot/actions)
<!-- badges: end -->

The goal of r2dii.ggplot is to provide users with plotting and data
processing functions that will allow the users to create standard 2DII
plots using `PACTA_analysis` or banks’ output data as input. The plots
are in the form of ggplot objects.

## Installation

You can install the development version of r2dii.ggplot from
[GitHub](https://github.com/2DegreesInvesting/r2dii.ggplot) with:

``` r
# install.packages("devtools")
devtools::install_github("2DegreesInvesting/r2dii.ggplot")
```

## Example

This is a basic example usage of `plot_trajectory_chart()`:

``` r
library(r2dii.ggplot)

example_data <- get_example_data()
example_data <- process_input_data(example_data)

data_trajectory <- filter_data_for_trajectory_chart(example_data,
  sector = "power", technology = "oilcap",
  region = "global", scenario_source = "demo_2020",
  value_name = "production", end_year = 2025,
  normalize_to_start_year = TRUE
)

scenario_specs <- data.frame(
  scenario = c("sds","sps","cps","worse"),
  color = c("#9CAB7C","#FFFFCC","#FDE291","#E07B73"),
  label = c("SDS","STEPS","CPS","worse")
  )
main_line_metric <- data.frame(metric = "projected", label = "Portfolio")
additional_line_metrics <- data.frame(metric = "corporate_economy", label = "Corporate Economy")

plot <- plot_trajectory_chart(data_trajectory,
  plot_title = "Production trajectory of Oil Capacity technology \n in the Power sector",
  x_title = "Year", y_title = "Production rate (normalized to 2020)",
  annotate_data = FALSE,
  scenario_specs_good_to_bad = scenario_specs,
  main_line_metric, additional_line_metrics
)

plot
```

<img src="man/figures/README-unnamed-chunk-2-1.png" width="100%" style="display: block; margin: auto auto auto 0;" />

This is a basic example usage of `plot_techmix_chart()`:

``` r
example_data <- get_example_data()
example_data <- process_input_data(example_data)

data_techmix_power <- filter_data_for_techmix_chart(example_data,
  sector = "power",
  years = c(2020, 2025), region = "global",
  scenario_source = "demo_2020",
  scenario = "sds", value_name = "technology_share"
)

tech_colours_power <- get_r2dii_technology_colours("power")
bars_labels_specs <- data.frame(
  "metric_type" = c("portfolio_2020", "benchmark_2020", "portfolio_2025", "benchmark_2025", "scenario_2025"),
  "label" = c("Portfolio 2020", "Benchmark 2020", "Portfolio 2025", "Benchmark 2025", "Target SDS 2025")
)

plot_techmix_power <- plot_techmix_chart(data_techmix_power, "Technology mix for the Power sector",
  show_legend = TRUE, tech_colours_power, bars_labels_specs
)

plot_techmix_power
```

<img src="man/figures/README-unnamed-chunk-3-1.png" width="100%" style="display: block; margin: auto auto auto 0;" />
