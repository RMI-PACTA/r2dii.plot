# r2dii.plot 0.5.2

* `market_share` and `sda` datasets renamed to `market_share_demo` and `sda_demo` respectively

# r2dii.plot 0.5.1

 * add definitions to `data_dictionary` (#597)

# r2dii.plot 0.5.0

## New features

* `data_dictionary` dataset added to define the columns in each dataset used or exported by the functions in this package (#576).

## Lifecycle changes

* `r2dii.plot` is now [stable](https://lifecycle.r-lib.org/articles/stages.html) (#587).

## Upkeep

* Handle deprecation of argument `"scale_name"` in `discrete_scale()` calls (deprecated in `ggplot2` 3.5.0) (#575).
* `r2dii.plot` updated for compatibility with `ggplot2` 3.6.0 (#575).

# r2dii.plot 0.4.0

## Breaking changes

* All `plot_*()` functions have had the data preparation step extracted into 
`prep_*()` functions. This means that from now on `prep_*()` function needs to be called on `data` prior to `plot_*()`. The APIs of `qplot_*()` functions remain unchanged (#465).
* `scale_colour_r2dii` has argument renamed from `labels` to `colour_labels` (#527).

## New features

* `techmix` plot does not show the start year scenario bar anymore. (#513)

## Bug fixes
* `scale_colour_r2dii`, with input colours and levels, now outputs line plot with correctly coloured lines (#527). 

# r2dii.plot 0.3.1

* Gains new datasets: 
  - `palette_colours` contains PACTA brand colours (hex codes), 
  - `scenario_colours` contains colours used for climate scenarios (hex codes), 
  - `sector_colours` contains PACTA sector colours (hex codes), 
  - `technology_colours` contains PACTA technology colours (hex codes).

* `r2dii.plot` has transferred to a new organization: 
https://github.com/RMI-PACTA/.

# r2dii.plot 0.3.0

## Breaking changes

* Package is updated to align with the newest version of `r2dii.analysis` 
(v 0.1.13).

* `qplot_trajectory()` now plots the column 
'percentage_of_initial_production_by_scope' by default and uses a percentage 
y-scale.

## New features

* `plot_trajectory()` gains new parameters: `value_col` to indicate which column 
contains the value to be plotted, and `perc_y_scale` to control whether y-axis 
should be scaled as percentage.

## Bug fixes and minor enhancements

* `to_title()` keeps the word unchanged if all letters are uppercase, for 
example: `to_title("SDS") = "SDS"`.

* `qplot_trajectory()` has new labels.

* `qplot_techmix()` has scenario name in label now (#476).

* All plots now start at the start year of 'projected' instead of first common 
year for all metrics. This allows for bar removal from 
techmix plots (#390 @Antoine-Lalechere).

# r2dii.plot 0.2.0

## New features

* `plot_*()` functions gain new parameters to help replicate some of
`qplot_*()` behaviors.

* New helpers to replicate with `plot_*()` functions the default labels of
`qplot_*()` functions:

    - `to_title()`
    - `format_metric()`
    - `recode_metric_techmix()`
    - `spell_out_technology()`

* New functions to easily apply 2DII colour palettes to any plot (#28
@cjyetman):

    - `scale_colour_r2dii()`
    - `scale_colour_r2dii_sector()`
    - `scale_colour_r2dii_tech()`
    - `scale_fill_r2dii()`
    - `scale_fill_r2dii_sector()`
    - `scale_fill_r2dii_tech()`
    
## Enhancements

* `plot_trajectory()` now defaults to not center the y-axis around the
start value.

* `plot_trajectory()` now displays benchmark trajectory lines with improved 
visibility (#389 @Antoine-Lalechere).

* The website now links to the blog where we advertise exciting news.

* The website now links to other r2dii packages (#381 @georgeharris2deg).

## Bug fixes

* `plot_trajectory()` now displays only integer years on the x-axis (#403).

* The 'Get started' section of the website works again.

* `theme_2dii()` now correctly uses the `base_size` argument.

# r2dii.plot 0.1.0

* `plot_techmix()` and `qplot_techmix()` always output the same bars' order
(#365).

* Data labels can be now modified by adding a column `label` and `label_tech`
to input data.

* The new `qplot_trajectory()`, `qplot_techmix()`, and
`qplot_emission_intensity()` output polished plots (#264).

* `plot_trajectory()`, `plot_techmix()`, and `plot_emission_intensity()`
now output more basic plots.

* Error messages now more closely follow the tidyverse style guide (#324).

* `plot_trajectory()` now plots the main line on top of all other lines,
so it's no longer hidden when it overlaps with other lines (#223).

* `plot_techmix()` for the "automotive" `sector` is now coloured so that
the darkest colours corresponds to the least sustainable technology, i.e.
consistently with other technologies. This pattern of decreasing sustainability
is now also reflected in the order of the legend labels (#328).

# r2dii.plot 0.0.1

* Initial release.
