# r2dii.plot 0.1.0

* `plot_techmix()` and `qplot_techmix()` always output the same bars' order (#365). 

* Data labels can be now modified by adding a column 'label' and 'label_tech' to
input data.

* The new `qplot_trajectory()`, `qplot_techmix()`, and `qplot_emission_intensity()`
output polished plots (#264).

* `plot_trajectory()`, `plot_techmix()`, and `plot_emission_intensity()` now output 
more basic plots.

* Error messages now more closely follow the tidyverse style guide (#324).  

* `plot_trajectory()` now plots the main line on top of all other lines, so
it's no longer hidden when it overlaps with other lines (#223).

* `plot_techmix()` for the "automotive" `sector` is now coloured so that the
darkest colours corresponds to the least sustainable technology, i.e.
consistently with other technologies. This pattern of decreasing
sustainability is now also reflected in the order of the legend labels (#328).

# r2dii.plot 0.0.1

* Initial release.
