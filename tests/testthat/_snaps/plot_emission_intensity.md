# if `data` is not a data frame errors gracefully

    is.data.frame(data) is not TRUE

# if `data` is not sda-like errors gracefully

    `data` must have all the expected names.
    x Missing names: emission_factor_metric, emission_factor_value.
    i Is your data `sda`-like?
    Caused by error in `abort_if_missing_names()`:
    ! `data` must have all the expected names.
    x Missing names: emission_factor_metric, emission_factor_value.

# if `data` has zero rows errors gracefully

    `zero_row` must have some rows.
    x `zero_row` has zero rows.

# with too many sectors errors gracefully

    `data` must have a single value of `sector`.
    i Do you need to pick one value? E.g. pick 'a' with: `subset(data, sector == 'a')`.
    x Provided: a, b.

# with too many lines to plot errors gracefully

    The number of lines to plot must be 7 or less.
    i Do you need to split the data over multiple plots?
    x Found 12 lines: projected, corporate_economy, target_demo, adjusted_scenario_demo, 1, 2, 3, 4, 5, 6, 7, 8.

# throws expected warning about API change

    The `data` argument of `plot_emission_intensity()` must be prepped already as of r2dii.plot 0.4.0.
    i From the next release you will need to call `r2dii.plot::plot_emission_intensity(data)` prior to calling `r2dii.plot::plot_emission_intensity()`.
    i Alternatively custom data preparation will also become possible.

