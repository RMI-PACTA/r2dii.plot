# if `data` is not a data frame errors gracefully

    rlang::is_named(data) is not TRUE

# if `data` is not sda-like errors gracefully

    `data` must have all the expected names.
    x Missing names: emission_factor_metric, emission_factor_value, label.
    i Is your data `sda`-like?
    Caused by error in `abort_if_missing_names()`:
    ! `data` must have all the expected names.
    x Missing names: emission_factor_metric, emission_factor_value, label.

# if `data` has zero rows errors gracefully

    `zero_row` must have some rows.
    x `zero_row` has zero rows.

# with too many sectors errors gracefully

    `data` must have all the expected names.
    x Missing names: label.
    i Is your data `sda`-like?
    Caused by error in `abort_if_missing_names()`:
    ! `data` must have all the expected names.
    x Missing names: label.

# with too many lines to plot errors gracefully

    The number of lines to plot must be 7 or less.
    i Do you need to split the data over multiple plots?
    x Found 12 lines: projected, corporate_economy, target_demo, adjusted_scenario_demo, 1, 2, 3, 4, 5, 6, 7, 8.

