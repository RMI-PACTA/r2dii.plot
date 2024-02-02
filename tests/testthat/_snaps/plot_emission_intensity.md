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

